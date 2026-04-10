use crate::{
    EbiObject, Exportable, Graphable, Importable, Infoable,
    line_reader::LineReader,
    traits::{
        graphable::{create_edge, create_place, create_transition},
        importable::{ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Error, Result, anyhow};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_activity_key::{
    Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
};
use ebi_arithmetic::{Fraction, One, Signed, Zero};
use ebi_derive::ActivityKey;
use layout::{core::base::Orientation, topo::layout::VisualGraph};
use std::{fmt::Display, io::BufRead};

pub const HEADER: &str = "finite stochastic partially ordered language";

#[derive(Clone, ActivityKey, Debug)]
pub struct FiniteStochasticPartiallyOrderedLanguage {
    pub(crate) activity_key: ActivityKey,
    pub traces: Vec<PartiallyOrderedTrace>,
    pub probabilities: Vec<Fraction>,
}

impl FiniteStochasticPartiallyOrderedLanguage {
    pub fn number_of_traces(&self) -> usize {
        self.traces.len()
    }
}

impl Importable for FiniteStochasticPartiallyOrderedLanguage {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A finite stochastic partially ordered language is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `finite stochastic partially ordered language'.
    The second line is the number of traces in the language.
    Then, for each trace, the following items are next, each on their own line.\\
    First, the probability of the trace.\\
    Second, the number of events in the trace.\\
    Third, for each event:
    \\begin{enumerate}
    \\item The activity.
    For a single-line event, escape a starting `\\$' by doubling it to `\\$'.
    For a multiline event, start with a line `\\$multiline', and end with a line `multiline\\$'
    To end any line end with `\\$', double it to `\\$\\$'.
    \\item The number of input edges,
    \\item For each input edge, on its own line, the index of the source event of the edge.
    \\end{enumerate}
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/model.sbpmn.spolang}";

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        Ok(EbiObject::FiniteStochasticPartiallyOrderedLanguage(
            Self::import(reader, parameter_values)?,
        ))
    }

    fn import(reader: &mut dyn BufRead, _parameter_values: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("Failed to read header, which should be `{}`.", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "First line should be exactly `{}`, but found `{}`.",
                HEADER,
                head
            ));
        }

        let number_of_traces = lreader
            .next_line_index()
            .with_context(|| anyhow!("Failed to read number of traces."))?;

        let mut sum = Fraction::zero();
        let mut traces = Vec::with_capacity(number_of_traces);
        let mut probabilities = Vec::with_capacity(number_of_traces);
        let mut activity_key = ActivityKey::new();

        for trace_i in 0..number_of_traces {
            let probability = lreader.next_line_weight().with_context(|| {
                format!(
                    "Failed to read weight for trace {} at line {}.",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            if !probability.is_positive() {
                return Err(anyhow!(
                    "Trace {} at line {} has a non-positive probability.",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            } else if probability > Fraction::one() {
                return Err(anyhow!(
                    "Trace {} at line {} has a probability higher than 1.",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            }

            sum += &probability;

            let number_of_events = lreader.next_line_index().with_context(|| {
                anyhow!(
                    "Failed to read number of events for trace {} at line {}.",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            let mut event_2_activity = Vec::with_capacity(number_of_events);
            let mut event_2_predecessors = Vec::with_capacity(number_of_events);
            for event_i in 0..number_of_events {
                let activity = lreader.next_activity(&mut activity_key).with_context(|| {
                    anyhow!(
                        "Reading activity of event {} of trace {} at line {}.",
                        event_i,
                        trace_i,
                        lreader.get_last_line_number()
                    )
                })?;
                event_2_activity.push(activity);

                let number_of_predecessors = lreader.next_line_index().with_context(|| {
                    anyhow!(
                        "Failed to read number of predecessors of event {} of trace {} at line {}.",
                        event_i,
                        trace_i,
                        lreader.get_last_line_number()
                    )
                })?;

                let mut predecessors = Vec::with_capacity(number_of_predecessors);
                for predecessor_i in 0..number_of_predecessors {
                    let predecessor = lreader.next_line_index().with_context(|| {
                        anyhow!(
                            "Failed to read predecessor {} of event {} of trace {} at line {}.",
                            predecessor_i,
                            event_i,
                            trace_i,
                            lreader.get_last_line_number()
                        )
                    })?;
                    if predecessor >= event_i {
                        return Err(anyhow!(
                            "The source {} of the predecessor {} of event {} of trace {} at line {} lies beyond its target event. Only forward-facing edges are allowed.",
                            predecessor,
                            predecessor_i,
                            event_i,
                            trace_i,
                            lreader.get_last_line_number()
                        ));
                    }
                    predecessors.push(predecessor);
                }
                event_2_predecessors.push(predecessors);
            }

            traces.push(PartiallyOrderedTrace {
                event_2_activity: event_2_activity,
                event_2_predecessors: event_2_predecessors,
            });
            probabilities.push(probability);
        }

        if sum > Fraction::one() && !sum.is_one() {
            //avoid rounding errors in approximate mode
            return Err(anyhow!(
                "The probabilities in the finite partially ordered stochastic language sum to {}, which is greater than 1.",
                sum
            ));
        }

        Ok(Self {
            activity_key: activity_key,
            traces,
            probabilities,
        })
    }
}
from_string!(FiniteStochasticPartiallyOrderedLanguage);

impl Exportable for FiniteStochasticPartiallyOrderedLanguage {
    fn export_from_object(object: EbiObject, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        match object {
            EbiObject::FiniteStochasticPartiallyOrderedLanguage(spolang) => spolang.export(f),
            _ => Err(anyhow!(
                "Cannot export {} {} as a finite stochastic partially ordered language.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Display for FiniteStochasticPartiallyOrderedLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of traces\n{}", self.number_of_traces())?;

        for (pos, (trace, probability)) in self
            .traces
            .iter()
            .zip(self.probabilities.iter())
            .enumerate()
        {
            writeln!(f, "# trace {}", pos)?;
            writeln!(f, "#\tprobability\n{}", probability)?;
            writeln!(f, "#\tnumber of events\n{}", trace.number_of_events())?;
            for (event_i, (activity, predecessors)) in trace
                .event_2_activity
                .iter()
                .zip(&trace.event_2_predecessors)
                .enumerate()
            {
                writeln!(f, "#\tevent {}", event_i)?;
                LineReader::write_multiline_activity(f, activity, &self.activity_key)?;
                writeln!(f, "#\t\tnumber of predecessors\n{}", predecessors.len())?;
                writeln!(f, "#\t\tpredecessors")?;
                for predecessor in predecessors {
                    writeln!(f, "{}", predecessor)?;
                }
            }
        }

        write!(f, "")
    }
}

impl Infoable for FiniteStochasticPartiallyOrderedLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.traces.len())?;
        writeln!(
            f,
            "Number of events\t{}",
            self.traces
                .iter()
                .map(|t| t.number_of_events())
                .sum::<usize>()
        )?;
        writeln!(
            f,
            "Number of edges\t{}",
            self.traces
                .iter()
                .map(|t| t.number_of_edges())
                .sum::<usize>()
        )?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key().get_number_of_activities()
        )?;
        writeln!(
            f,
            "Sum of probabilities\t{:.4}",
            self.probabilities
                .iter()
                .fold(Fraction::zero(), |mut x, y| {
                    x += y;
                    x
                })
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(writeln!(f, "")?)
    }
}

impl Graphable for FiniteStochasticPartiallyOrderedLanguage {
    fn to_dot(&self) -> Result<VisualGraph> {
        let mut graph = VisualGraph::new(Orientation::TopToBottom);

        for (trace, probability) in self.traces.iter().zip(self.probabilities.iter()) {
            trace.to_dot(probability, &self.activity_key, &mut graph);
        }

        Ok(graph)
    }
}

impl TranslateActivityKey for FiniteStochasticPartiallyOrderedLanguage {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        self.traces.iter_mut().for_each(|trace| {
            trace.event_2_activity.iter_mut().for_each(|activity| {
                *activity = translator.translate_activity(activity);
            })
        });

        self.activity_key = to_activity_key.clone();
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for FiniteStochasticPartiallyOrderedLanguage {
    fn test_activity_key(&self) {
        self.traces.iter().for_each(|trace| {
            trace
                .event_2_activity
                .iter()
                .for_each(|activity| self.activity_key().assert_activity_is_of_key(activity))
        });
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PartiallyOrderedTrace {
    pub event_2_activity: Vec<Activity>,
    pub event_2_predecessors: Vec<Vec<usize>>,
}

impl PartiallyOrderedTrace {
    pub fn number_of_events(&self) -> usize {
        self.event_2_activity.len()
    }

    pub fn number_of_edges(&self) -> usize {
        self.event_2_predecessors.iter().map(|l| l.len()).sum()
    }

    /**
     * Returns the activity of the event. Returns None if the event does not exist.
     */
    pub fn get_event_activity(&self, event: usize) -> Option<&Activity> {
        self.event_2_activity.get(event)
    }

    pub fn start_events(&self) -> impl Iterator<Item = usize> {
        self.event_2_predecessors
            .iter()
            .enumerate()
            .filter_map(|(index, predecessors)| {
                if predecessors.is_empty() {
                    Some(index)
                } else {
                    None
                }
            })
    }

    pub fn to_dot(
        &self,
        probability: &Fraction,
        activity_key: &ActivityKey,
        graph: &mut VisualGraph,
    ) {
        //events
        let event_2_dot = self
            .event_2_activity
            .iter()
            .map(|activity| {
                let label = activity_key.deprocess_activity(activity);
                create_transition(graph, label, "")
            })
            .collect::<Vec<_>>();

        //start event
        let start_event = create_place(graph, &format!("{:.4}", probability));
        for event in self.start_events() {
            create_edge(graph, &start_event, &event_2_dot[event], "");
        }

        //edges
        for target in 0..self.number_of_edges() {
            for source in &self.event_2_predecessors[target] {
                create_edge(graph, &event_2_dot[*source], &event_2_dot[target], "");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::FiniteStochasticPartiallyOrderedLanguage;
    use std::fs;

    #[test]
    fn spolang_import() {
        let fin = fs::read_to_string("testfiles/flower.sbpmn.spolang").unwrap();
        let _spolang = fin
            .parse::<FiniteStochasticPartiallyOrderedLanguage>()
            .unwrap();
    }
}
