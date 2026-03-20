use crate::{
    EbiObject, Exportable, Graphable, Importable, Infoable,
    line_reader::LineReader,
    traits::{
        graphable::{
            create_dot, create_edge, create_place, create_silent_transition, create_transition,
        },
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
    Then, for each trace, the following items are next, each on their own line.
    First, the probability of the trace.
    Second, the number of edges in the trace.
    Third, for each edge, 
    (i) the word `silent' or the word `label ' directly followed by the activity label, or the phrase 'multiline label ' followed by the label, terminated with `\\#' (a `\\#' at the line end can be escaped with `\\#\\#').
    (ii) the number of input states,
    (iii) the input states, 
    (iv) the number of output states, and 
    (v) the output states.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.lpn}";

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

            let number_of_edges = lreader.next_line_index().with_context(|| {
                format!(
                    "Failed to read number of edges for trace {} at line {}.",
                    trace_i,
                    lreader.get_last_line_number(),
                )
            })?;

            let mut edge_2_inputs = Vec::with_capacity(number_of_edges);
            let mut edge_2_outputs = Vec::with_capacity(number_of_edges);
            let mut edge_2_activity = Vec::with_capacity(number_of_edges);
            let mut max_state = 0;

            for edge_i in 0..number_of_edges {
                let activity =
                    read_activity(&mut lreader, &mut activity_key).with_context(|| {
                        anyhow!(
                            "Reading activity of edge {} of trace {} at line {}.",
                            edge_i,
                            trace_i,
                            lreader.get_last_line_number()
                        )
                    })?;
                edge_2_activity.push(activity);

                //inputs
                {
                    let number_of_inputs = lreader.next_line_index().with_context(|| {
                        format!(
                            "Failed to read number of input states for edge {} of trace {} at line {}.",
                            edge_i,
                            trace_i,
                            lreader.get_last_line_number(),
                        )
                    })?;

                    let mut inputs = Vec::with_capacity(number_of_inputs);
                    for input_i in 0..number_of_inputs {
                        let input = lreader.next_line_index().with_context(|| {
                            anyhow!(
                                "Failed to read input state {} of edge {} of trace {} at line {}.",
                                input_i,
                                edge_i,
                                trace_i,
                                lreader.get_last_line_number()
                            )
                        })?;
                        max_state = max_state.max(input);
                        inputs.push(input);
                    }
                    edge_2_inputs.push(inputs);
                }

                //outputs
                {
                    let number_of_outputs = lreader.next_line_index().with_context(|| {
                        format!(
                            "Failed to read number of output states for edge {} of trace {} at line {}.",
                            edge_i,
                            trace_i,
                            lreader.get_last_line_number(),
                        )
                    })?;

                    let mut outputs = Vec::with_capacity(number_of_outputs);
                    for output_i in 0..number_of_outputs {
                        let output = lreader.next_line_index().with_context(|| {
                            anyhow!(
                                "Failed to read output state {} of edge {} of trace {} at line {}.",
                                output_i,
                                edge_i,
                                trace_i,
                                lreader.get_last_line_number()
                            )
                        })?;
                        max_state = max_state.max(output);
                        outputs.push(output);
                    }
                    edge_2_outputs.push(outputs);
                }
            }

            traces.push(PartiallyOrderedTrace {
                number_of_states: max_state + 1,
                edge_2_inputs,
                edge_2_outputs,
                edge_2_activity,
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
            writeln!(f, "#\tnumber of edges\n{}", trace.number_of_edges())?;
            for edge in 0..trace.number_of_edges() {
                writeln!(f, "#\tedge {}", edge)?;
                write_activity(f, trace.get_edge_activity(edge), &self.activity_key)?;
                writeln!(
                    f,
                    "#\t\tnumber of input states\n{}",
                    trace.edge_2_inputs[edge].len()
                )?;
                for input in &trace.edge_2_inputs[edge] {
                    writeln!(f, "{}", input)?;
                }
                writeln!(
                    f,
                    "#\t\tnumber of output states\n{}",
                    trace.edge_2_outputs[edge].len()
                )?;
                for output in &trace.edge_2_outputs[edge] {
                    writeln!(f, "{}", output)?;
                }
            }
        }

        write!(f, "")
    }
}

pub fn write_activity(
    f: &mut std::fmt::Formatter<'_>,
    activity: Option<Activity>,
    activity_key: &ActivityKey,
) -> std::fmt::Result {
    if let Some(activity) = activity {
        let activity_label = activity_key.get_activity_label(&activity);
        if activity_label.contains("\n") {
            let activity_label = activity_label.replace("#\n", "##\n");
            writeln!(f, "multiline label {}#", activity_label)
        } else {
            writeln!(f, "label {}", activity_label)
        }
    } else {
        writeln!(f, "silent")
    }
}

pub fn read_activity(
    lreader: &mut LineReader,
    activity_key: &mut ActivityKey,
) -> Result<Option<Activity>> {
    let line = lreader
        .next_line_string()
        .with_context(|| format!("Failed to read activity."))?;

    if line.trim_start().starts_with("label ") {
        let label = line.trim_start()[6..].to_string();
        let activity = activity_key.process_activity(&label);
        Ok(Some(activity))
    } else if line.trim_start().starts_with("multiline label ") {
        let mut label = line.trim_start()[16..].to_string();
        while !label.ends_with("#") && !label.ends_with("##") {
            label += "\n";
            label += &lreader.next_line_string().with_context(|| {
                anyhow!("Was expecting a line ending with `#' to end a multiline label.")
            })?;
        }
        label.pop(); //remove last #
        label = label.replace("##\n", "#\n"); //un-escape ## at the end of the line to #

        let activity = activity_key.process_activity(&label);
        Ok(Some(activity))
    } else {
        //silent
        Ok(None)
    }
}

impl Infoable for FiniteStochasticPartiallyOrderedLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.traces.len())?;
        writeln!(
            f,
            "Number of states\t{}",
            self.traces
                .iter()
                .map(|t| t.number_of_states())
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
            trace.edge_2_activity.iter_mut().for_each(|activity| {
                if let Some(activity) = activity {
                    *activity = translator.translate_activity(activity);
                }
            })
        });

        self.activity_key = to_activity_key.clone();
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for FiniteStochasticPartiallyOrderedLanguage {
    fn test_activity_key(&self) {
        self.traces.iter().for_each(|trace| {
            trace.edge_2_activity.iter().for_each(|activity| {
                if let Some(activity) = activity {
                    self.activity_key().assert_activity_is_of_key(activity)
                }
            })
        });
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PartiallyOrderedTrace {
    pub number_of_states: usize,
    pub edge_2_inputs: Vec<Vec<usize>>,
    pub edge_2_outputs: Vec<Vec<usize>>,
    pub edge_2_activity: Vec<Option<Activity>>,
}

impl PartiallyOrderedTrace {
    pub fn number_of_states(&self) -> usize {
        self.number_of_states
    }

    pub fn number_of_edges(&self) -> usize {
        self.edge_2_activity.len()
    }

    pub fn get_edge_activity(&self, edge: usize) -> Option<Activity> {
        *self.edge_2_activity.get(edge)?
    }

    pub fn to_dot(
        &self,
        probability: &Fraction,
        activity_key: &ActivityKey,
        graph: &mut VisualGraph,
    ) {
        //states
        let state_2_node = (0..self.number_of_states())
            .map(|_| create_dot(graph))
            .collect::<Vec<_>>();

        //start state
        let start_state = create_place(graph, &format!("{:.4}", probability));
        for state in 0..self.number_of_states() {
            if !self
                .edge_2_outputs
                .iter()
                .any(|outputs| outputs.contains(&state))
            {
                create_edge(graph, &start_state, &state_2_node[state], "");
            }
        }

        //edges
        for edge in 0..self.number_of_edges() {
            //midpoint
            let midpoint = if let Some(activity) = self.edge_2_activity[edge] {
                create_transition(graph, activity_key.deprocess_activity(&activity), "")
            } else {
                create_silent_transition(graph, "")
            };

            //connections
            for input in &self.edge_2_inputs[edge] {
                create_edge(graph, &state_2_node[*input], &midpoint, "");
            }
            for output in &self.edge_2_outputs[edge] {
                create_edge(graph, &midpoint, &state_2_node[*output], "");
            }
        }
    }
}
