use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, EbiObject, Exportable, Graphable, HasActivityKey,
    Importable, Infoable, TranslateActivityKey,
    ebi_objects::labelled_petri_net::TransitionIndex,
    line_reader::LineReader,
    marking::Marking,
    traits::{
        graphable,
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Result, anyhow};
use ebi_arithmetic::Fraction;
use ebi_derive::ActivityKey;
use layout::topo::layout::VisualGraph;
use std::{fmt, io::BufRead};

use super::labelled_petri_net::LabelledPetriNet;

pub const HEADER: &str = "stochastic labelled Petri net";

#[derive(Clone, Debug, ActivityKey)]
pub struct StochasticLabelledPetriNet {
    pub activity_key: ActivityKey,
    pub initial_marking: Marking,
    pub labels: Vec<Option<Activity>>,
    pub transition2input_places: Vec<Vec<usize>>,
    pub transition2output_places: Vec<Vec<usize>>,
    pub transition2input_places_cardinality: Vec<Vec<u64>>,
    pub transition2output_places_cardinality: Vec<Vec<u64>>, //fields are the same as in LabelledPetriNet
    pub place2output_transitions: Vec<Vec<usize>>,
    pub weights: Vec<Fraction>,
}

impl StochasticLabelledPetriNet {
    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            initial_marking: Marking::new(0),
            labels: vec![],
            place2output_transitions: vec![],
            transition2input_places: vec![],
            transition2output_places: vec![],
            transition2input_places_cardinality: vec![],
            transition2output_places_cardinality: vec![],
            weights: vec![],
        }
    }

    pub fn new_empty_language() -> Self {
        return Self {
            activity_key: ActivityKey::new(),
            initial_marking: Marking::new(0),
            labels: vec![None],
            transition2input_places: vec![vec![]],
            transition2output_places: vec![vec![]],
            transition2input_places_cardinality: vec![vec![]],
            transition2output_places_cardinality: vec![vec![]],
            place2output_transitions: vec![],
            weights: vec![],
        };
    }

    pub fn get_number_of_places(&self) -> usize {
        self.place2output_transitions.len()
    }

    pub fn get_initial_marking(&self) -> &Marking {
        &self.initial_marking
    }

    pub fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        self.labels[transition].is_none()
    }

    pub fn get_transition_label(&self, transition: TransitionIndex) -> Option<Activity> {
        self.labels[transition]
    }

    pub fn incidence_vector(&self, transition: TransitionIndex) -> Vec<i128> {
        let mut vec2 = vec![0; self.get_number_of_places()];
        for (in_place_pos, in_place) in self.transition2input_places[transition].iter().enumerate()
        {
            vec2[*in_place] -=
                self.transition2input_places_cardinality[transition][in_place_pos] as i128;
        }
        for (out_place_pos, out_place) in
            self.transition2output_places[transition].iter().enumerate()
        {
            vec2[*out_place] +=
                self.transition2output_places_cardinality[transition][out_place_pos] as i128;
        }
        vec2
    }

    pub fn max_transition_input_arc_cardinality(&self) -> u64 {
        if let Some(x) = self
            .transition2input_places_cardinality
            .iter()
            .flatten()
            .max()
        {
            *x
        } else {
            0
        }
    }

    pub fn add_place(&mut self) -> usize {
        let place = self.get_number_of_places();
        self.place2output_transitions.push(vec![]);
        self.initial_marking.add_place();
        place
    }

    pub fn get_initial_marking_mut(&mut self) -> &mut Marking {
        &mut self.initial_marking
    }

    pub fn add_transition(&mut self, label: Option<Activity>, weight: Fraction) -> TransitionIndex {
        self.labels.push(label);
        self.transition2input_places.push(vec![]);
        self.transition2input_places_cardinality.push(vec![]);
        self.transition2output_places.push(vec![]);
        self.transition2output_places_cardinality.push(vec![]);
        self.weights.push(weight);
        self.transition2input_places.len() - 1
    }

    pub fn add_transition_place_arc(
        &mut self,
        from_transition: TransitionIndex,
        to_place: usize,
        cardinality: u64,
    ) -> Result<()> {
        if from_transition >= self.transition2input_places.len() {
            return Err(anyhow!(
                "non-existing transition {} referenced, while there are {}",
                from_transition,
                self.transition2input_places.len()
            ));
        } else if to_place >= self.get_number_of_places() {
            return Err(anyhow!(
                "non-existing place {} referenced, while there are {}",
                to_place,
                self.get_number_of_places()
            ));
        }

        if let Some(pos) = self.transition2output_places[from_transition]
            .iter()
            .position(|p| *p == to_place)
        {
            self.transition2output_places_cardinality[from_transition][pos] += cardinality;
        } else {
            self.transition2output_places[from_transition].push(to_place);
            self.transition2output_places_cardinality[from_transition].push(1);
        }
        Ok(())
    }

    pub fn add_place_transition_arc(
        &mut self,
        from_place: usize,
        to_transition: TransitionIndex,
        cardinality: u64,
    ) -> Result<()> {
        if to_transition >= self.transition2input_places.len() {
            return Err(anyhow!(
                "non-existing transition {} referenced, while there are {}",
                to_transition,
                self.transition2input_places.len()
            ));
        } else if from_place >= self.get_number_of_places() {
            return Err(anyhow!(
                "non-existing place {} referenced, while there are {}",
                from_place,
                self.get_number_of_places()
            ));
        }

        self.place2output_transitions[from_place].push(to_transition);

        if let Some(pos) = self.transition2input_places[to_transition]
            .iter()
            .position(|p| *p == from_place)
        {
            self.transition2input_places_cardinality[to_transition][pos] += cardinality;
        } else {
            self.transition2input_places[to_transition].push(from_place);
            self.transition2input_places_cardinality[to_transition].push(1);
        }
        Ok(())
    }
}

impl TranslateActivityKey for StochasticLabelledPetriNet {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.labels.iter_mut().for_each(|activity| {
            if let Some(a) = activity {
                *a = translator.translate_activity(&a)
            }
        });
        self.activity_key = to_activity_key.clone();
    }
}

impl Exportable for StochasticLabelledPetriNet {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiObject::StochasticLabelledPetriNet(log) => log.export(f),
            EbiObject::StochasticDirectlyFollowsModel(log) => {
                Into::<StochasticLabelledPetriNet>::into(log).export(f)
            }
            EbiObject::StochasticNondeterministicFiniteAutomaton(snfa) => {
                Into::<StochasticLabelledPetriNet>::into(snfa).export(f)
            }
            EbiObject::StochasticDeterministicFiniteAutomaton(sdfa) => {
                Into::<StochasticLabelledPetriNet>::into(sdfa).export(f)
            }
            EbiObject::DirectlyFollowsGraph(log) => {
                Into::<StochasticLabelledPetriNet>::into(log).export(f)
            }
            _ => Err(anyhow!(
                "cannot export {} {} as an SLPN",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for StochasticLabelledPetriNet {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of places\t\t{}", self.get_number_of_places())?;
        writeln!(
            f,
            "Number of transitions\t\t{}",
            self.transition2input_places.len()
        )?;
        writeln!(
            f,
            "Number of activities\t\t{}",
            self.activity_key().get_number_of_activities()
        )?;
        writeln!(
            f,
            "Number of silent transitions\t{}",
            (0..self.transition2input_places.len())
                .into_iter()
                .filter(|transition| self.is_transition_silent(*transition))
                .count()
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

impl fmt::Display for StochasticLabelledPetriNet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of places\n{}", self.get_number_of_places())?;

        writeln!(f, "# initial marking")?;
        for place in self.initial_marking.get_place2token() {
            writeln!(f, "{}", place)?;
        }

        writeln!(
            f,
            "# number of transitions\n{}",
            self.transition2input_places.len()
        )?;

        for transition in 0..self.transition2input_places.len() {
            writeln!(f, "# transition {}", transition)?;

            if let Some(activity) = self.get_transition_label(transition) {
                writeln!(
                    f,
                    "label {}",
                    self.activity_key.get_activity_label(&activity)
                )?;
            } else {
                writeln!(f, "silent")?;
            }

            writeln!(f, "# weight\n{}", &self.weights[transition])?;

            writeln!(
                f,
                "# number of input places\n{}",
                self.transition2input_places[transition].len()
            )?;
            for (pos, place) in self.transition2input_places[transition].iter().enumerate() {
                for _ in 0..self.transition2input_places_cardinality[transition][pos] {
                    writeln!(f, "{}", place)?;
                }
            }

            writeln!(
                f,
                "# number of output places\n{}",
                self.transition2output_places[transition].len()
            )?;
            for (pos, place) in self.transition2output_places[transition].iter().enumerate() {
                for _ in 0..self.transition2output_places_cardinality[transition][pos] {
                    writeln!(f, "{}", place)?;
                }
            }
        }

        write!(f, "")
    }
}

impl Importable for StochasticLabelledPetriNet {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A stochastic labelled Petri net is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `stochastic labelled Petri net'.
    The second line is the number of places in the net.
    The lines thereafter contain the initial marking: each place has its own line with the number of tokens on that place in the initial marking.
    The next line is the number of transitions in the net.
    Then, for each transition, the following lines are next: 
    (i) the word `silent' or the word `label' followed by a space and the name of the activity with which the transition is labelled;
    (ii) the weight of the transition, which may be any fraction or decimal number, even 0 or negative;
    (iii) the number of input places, followed by a line for each input place with the index of the place;
    (iiii) the number of output places, followed by a line for each output place with the index of the place.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba_ali.slpn}";

    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::StochasticLabelledPetriNet(Self::import(
            reader,
            parameter_values,
        )?))
    }

    fn import(reader: &mut dyn BufRead, _: &ImporterParameterValues) -> Result<Self> {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}` on line `{}`",
                HEADER,
                lreader.get_last_line(),
                lreader.get_last_line_number()
            ));
        }

        let mut activity_key = ActivityKey::new();

        let number_of_places = lreader
            .next_line_index()
            .context("failed to read number of places")?;
        let mut place2output_transitions = vec![vec![]; number_of_places];

        //read initial marking
        let mut initial_marking = vec![0u64; number_of_places];
        for place in 0..number_of_places {
            initial_marking[place] = lreader
                .next_line_natural()
                .with_context(|| format!("failed to read initial marking of place {}", place))?;
        }

        //read transitions
        let number_of_transitions = lreader
            .next_line_index()
            .context("failed to read number of transitions")?;

        let mut weights = Vec::with_capacity(number_of_transitions);
        let mut labels = Vec::with_capacity(number_of_transitions);
        let mut transition2input_places: Vec<Vec<usize>> = vec![vec![]; number_of_transitions];
        let mut transition2output_places: Vec<Vec<usize>> = vec![vec![]; number_of_transitions];
        let mut transition2input_places_cardinality: Vec<Vec<u64>> =
            vec![vec![]; number_of_transitions];
        let mut transition2output_places_cardinality: Vec<Vec<u64>> =
            vec![vec![]; number_of_transitions];

        for transition in 0..number_of_transitions {
            let label_line = lreader
                .next_line_string()
                .with_context(|| format!("failed to read label of transition {}", transition))?;

            //read label
            if label_line.trim_start().starts_with("label ") {
                let label = label_line.trim_start()[6..].to_string();
                let activity = activity_key.process_activity(&label);
                labels.push(Some(activity));
            } else {
                labels.push(None);
            }

            //read weight
            {
                let weight = lreader.next_line_weight().with_context(|| {
                    format!("failed to read weight of transition {}", transition)
                })?;
                weights.push(weight);
            }

            //read input places
            {
                let number_of_input_places = lreader.next_line_index().with_context(|| {
                    format!(
                        "Failed to read number of input places of transition {}.",
                        transition
                    )
                })?;
                for p in 0..number_of_input_places {
                    let place = lreader.next_line_index().with_context(|| {
                        format!(
                            "Failed to read input place number {} of transition {}.",
                            p, transition
                        )
                    })?;

                    if place >= number_of_places {
                        return Err(anyhow!(
                            "Non-existing place referenced for transition {}, input place number {}, at line {}; found `{}`.",
                            transition,
                            p,
                            lreader.get_last_line_number(),
                            lreader.get_last_line()
                        ));
                    }

                    place2output_transitions[place].push(transition);

                    if let Some(pos) = transition2input_places[transition]
                        .iter()
                        .position(|p| *p == place)
                    {
                        transition2input_places_cardinality[transition][pos] += 1;
                    } else {
                        transition2input_places[transition].push(place);
                        transition2input_places_cardinality[transition].push(1);
                    }
                }
            }

            //read output places
            {
                let number_of_output_places = lreader.next_line_index().with_context(|| {
                    format!(
                        "failed to read number of output places of transition {}",
                        transition
                    )
                })?;
                for p in 0..number_of_output_places {
                    let place = lreader.next_line_index().with_context(|| {
                        format!(
                            "failed to read output place number {} of transition {}",
                            p, transition
                        )
                    })?;

                    if place >= number_of_places {
                        return Err(anyhow!(
                            "non-existing place referenced for transition {}, output place number {}, at line {}; found `{}`",
                            transition,
                            p,
                            lreader.get_last_line_number(),
                            lreader.get_last_line()
                        ));
                    }

                    if let Some(pos) = transition2output_places[transition]
                        .iter()
                        .position(|p| *p == place)
                    {
                        transition2output_places_cardinality[transition][pos] += 1;
                    } else {
                        transition2output_places[transition].push(place);
                        transition2output_places_cardinality[transition].push(1);
                    }
                }
            }
        }

        Ok(StochasticLabelledPetriNet {
            activity_key,
            initial_marking: Marking {
                place2token: initial_marking,
            },
            labels,
            place2output_transitions,
            transition2input_places,
            transition2output_places,
            transition2input_places_cardinality,
            transition2output_places_cardinality,
            weights,
        })
    }
}
from_string!(StochasticLabelledPetriNet);

impl From<(LabelledPetriNet, Vec<Fraction>)> for StochasticLabelledPetriNet {
    fn from(value: (LabelledPetriNet, Vec<Fraction>)) -> Self {
        Self {
            activity_key: value.0.activity_key,
            initial_marking: value.0.initial_marking,
            labels: value.0.labels,
            place2output_transitions: value.0.place2output_transitions,
            transition2input_places: value.0.transition2input_places,
            transition2output_places: value.0.transition2output_places,
            transition2input_places_cardinality: value.0.transition2input_places_cardinality,
            transition2output_places_cardinality: value.0.transition2output_places_cardinality,
            weights: value.1,
        }
    }
}

impl Graphable for StochasticLabelledPetriNet {
    fn to_dot(&self) -> Result<VisualGraph> {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for place in 0..self.get_number_of_places() {
            let label = if let Some(marked) = self.initial_marking.place2token.get(place) {
                if marked > &0 {
                    marked.to_string()
                } else {
                    "".to_string()
                }
            } else {
                "".to_string()
            };

            places.push(graphable::create_place(&mut graph, &label));
        }

        for transition in 0..self.transition2input_places.len() {
            let node = if let Some(activity) = self.get_transition_label(transition) {
                graphable::create_transition(
                    &mut graph,
                    self.activity_key.get_activity_label(&activity),
                    &self.weights[transition].to_string(),
                )
            } else {
                graphable::create_silent_transition(
                    &mut graph,
                    &self.weights[transition].to_string(),
                )
            };

            for (pos, inplace) in self.transition2input_places[transition].iter().enumerate() {
                let place_node = places.get(*inplace).unwrap();
                if self.transition2input_places_cardinality[transition][pos] > 1 {
                    graphable::create_edge(
                        &mut graph,
                        place_node,
                        &node,
                        &format!(
                            "{}",
                            self.transition2input_places_cardinality[transition][pos]
                        ),
                    );
                } else {
                    graphable::create_edge(&mut graph, place_node, &node, "");
                }
            }

            for (pos, outplace) in self.transition2output_places[transition].iter().enumerate() {
                let place_node = places.get(*outplace).unwrap();
                if self.transition2output_places_cardinality[transition][pos] > 1 {
                    graphable::create_edge(
                        &mut graph,
                        &node,
                        place_node,
                        &format!(
                            "{}",
                            self.transition2output_places_cardinality[transition][pos]
                        ),
                    );
                } else {
                    graphable::create_edge(&mut graph, &node, place_node, "");
                }
            }
        }

        Ok(graph)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::ebi_objects::stochastic_labelled_petri_net::StochasticLabelledPetriNet;

    #[test]
    fn empty_slpn() {
        let fin = fs::read_to_string("testfiles/empty_net.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert_eq!(slpn.get_number_of_places(), 0);
        assert_eq!(slpn.transition2input_places.len(), 0);
    }
}
