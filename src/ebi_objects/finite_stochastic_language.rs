use anyhow::{Context, Error, Result, anyhow};
use ebi_arithmetic::{Fraction, One, Signed, Zero};
use ebi_derive::ActivityKey;
use std::{
    collections::HashMap,
    fmt,
    io::{self, BufRead, Write},
    str::FromStr,
};

use crate::{
    Activity, ActivityKey, ActivityKeyTranslator, Exportable, HasActivityKey, Importable,
    IndexTrace, Infoable, TranslateActivityKey, constants::ebi_object::EbiObject,
    line_reader::LineReader,
};

pub const HEADER: &str = "finite stochastic language";

pub const FORMAT_SPECIFICATION: &str = "A finite language is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `finite stochastic language'.
    The second line is the number of traces in the language.
    For each trace, the first line is the probability of the trace as a positive fraction or a decimal value.
    The second line contains the number of events in the trace.
    Then, each subsequent line contains the activity name of one event.

    The sum of the probabilities of the traces in the language needs to be $\\leq$ 1.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.slang}";

#[derive(Clone, Debug, ActivityKey)]
pub struct FiniteStochasticLanguage {
    pub activity_key: ActivityKey,
    pub traces: HashMap<Vec<Activity>, Fraction>,
}

impl FiniteStochasticLanguage {
    /**
     * Does not normalise the distribution.
     */
    pub fn new_raw(traces: HashMap<Vec<Activity>, Fraction>, activity_key: ActivityKey) -> Self {
        Self {
            activity_key: activity_key,
            traces: traces,
        }
    }

    pub fn normalise_before(traces: &mut HashMap<Vec<String>, Fraction>) {
        if traces.len() != 0 {
            let sum = traces.values().fold(Fraction::zero(), |mut x, y| {
                x += y;
                x
            });
            log::info!("the extracted traces cover a sum of {}", sum);
            traces.retain(|_, v| {
                *v /= &sum;
                true
            });
        }
    }

    pub fn normalise(&mut self) {
        if self.number_of_traces() != 0 {
            let sum = self.traces.values().fold(Fraction::zero(), |mut x, y| {
                x += y;
                x
            });
            log::info!("the extracted traces cover a sum of {}", sum);
            self.traces.retain(|_, v| {
                *v /= &sum;
                true
            });
        }
    }

    fn contains(&self, atrace_b: Vec<&str>, probability_b: &Fraction) -> bool {
        for trace_a in self.traces.iter() {
            let atrace_a = self.activity_key.deprocess_trace(&trace_a.0);

            if atrace_a == atrace_b && trace_a.1 == probability_b {
                return true;
            }
        }
        return false;
    }
}

impl TranslateActivityKey for FiniteStochasticLanguage {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        //a hashmap needs to be rebuilt, unfortunately
        let translated_traces: HashMap<Vec<Activity>, Fraction> = self
            .traces
            .drain() // `drain` is used to take ownership of the original traces (use `into_iter()` or `drain()` if we want to consume)
            .map(|(trace, fraction)| (translator.translate_trace(&trace), fraction))
            .collect();

        // Update the traces in the language with the translated ones
        self.traces = translated_traces;

        self.activity_key = to_activity_key.clone();
    }
}

impl Eq for FiniteStochasticLanguage {}

impl PartialEq for FiniteStochasticLanguage {
    fn eq(&self, other: &Self) -> bool {
        if self.traces.len() != other.traces.len() {
            return false;
        }
        for trace_b in other.traces.iter() {
            let atrace_b = other.activity_key.deprocess_trace(&trace_b.0);
            if !self.contains(atrace_b, trace_b.1) {
                return false;
            }
        }

        return true;
    }
}

impl From<HashMap<Vec<String>, Fraction>> for FiniteStochasticLanguage {
    /**
     * Normalises the distribution. Use new_raw to avoid normalisation.
     */
    fn from(mut value: HashMap<Vec<String>, Fraction>) -> Self {
        Self::normalise_before(&mut value);
        let mut activity_key = ActivityKey::new();
        let a_traces = value
            .into_iter()
            .map(|(trace, probability)| (activity_key.process_trace(&trace), probability))
            .collect();
        Self {
            activity_key: activity_key,
            traces: a_traces,
        }
    }
}

impl From<(ActivityKey, HashMap<Vec<Activity>, Fraction>)> for FiniteStochasticLanguage {
    /**
     * Normalises the distribution. Use new_raw to avoid normalisation.
     */
    fn from(value: (ActivityKey, HashMap<Vec<Activity>, Fraction>)) -> Self {
        let mut result = Self {
            activity_key: value.0,
            traces: value.1,
        };
        result.normalise();
        result
    }
}

impl FromStr for FiniteStochasticLanguage {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for FiniteStochasticLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::FiniteStochasticLanguage(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be `{}`", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                head
            ));
        }

        let number_of_traces = lreader
            .next_line_index()
            .context("failed to read number of places")?;

        let mut traces = HashMap::new();
        let mut sum = Fraction::zero();
        let mut activity_key = ActivityKey::new();
        for trace_i in 0..number_of_traces {
            let probability = lreader.next_line_weight().with_context(|| {
                format!(
                    "failed to read weight for trace {} at line {}",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            if !probability.is_positive() {
                return Err(anyhow!(
                    "trace {} at line {} has non-positive probability",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            } else if probability > Fraction::one() {
                return Err(anyhow!(
                    "trace {} at line {} has a probability higher than 1",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            }

            sum += probability.clone();

            let number_of_events = lreader.next_line_index().with_context(|| {
                format!(
                    "failed to read number of events for trace {} at line {}",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            let mut trace = vec![];
            trace.reserve_exact(number_of_events);

            for event_i in 0..number_of_events {
                let event = lreader.next_line_string().with_context(|| {
                    format!(
                        "failed to read event {} of trace {} at line {}",
                        event_i,
                        trace_i,
                        lreader.get_last_line_number()
                    )
                })?;
                trace.push(event);
            }

            let trace = activity_key.process_trace(&trace);
            if traces.insert(trace, probability).is_some() {
                return Err(anyhow!(
                    "trace {} ending at line {} appears twice in language",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            }
        }

        if sum > Fraction::one() && !sum.is_one() {
            //avoid rounding errors in approximate mode
            return Err(anyhow!(
                "probabilities in stochastic language sum to {}, which is greater than 1",
                sum
            ));
        }

        Ok(Self {
            activity_key: activity_key,
            traces: traces,
        })
    }
}

impl Exportable for FiniteStochasticLanguage {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::FiniteStochasticLanguage(slang) => slang.export(f),
            EbiObject::EventLog(log) => Into::<Self>::into(log).export(f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for FiniteStochasticLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.traces.len())?;
        writeln!(
            f,
            "Number of events\t{}",
            self.traces.iter().map(|t| t.0.len()).sum::<usize>()
        )?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key().get_number_of_activities()
        )?;
        writeln!(
            f,
            "Sum of probabilities\t{:.4}",
            self.traces.values().fold(Fraction::zero(), |mut x, y| {
                x += y;
                x
            })
        )?;

        writeln!(f, "")?;
        self.activity_key().info(f)?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for FiniteStochasticLanguage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of traces\n{}", self.traces.len())?;

        for (pos, (trace, probability)) in self.traces.iter().enumerate() {
            writeln!(f, "# trace {}", pos)?;

            writeln!(f, "# probability\n{}", probability)?;

            writeln!(f, "# number of events\n{}", trace.len())?;
            for event in trace {
                writeln!(f, "{}", self.activity_key.get_activity_label(event))?;
            }
        }

        write!(f, "")
    }
}

impl IndexTrace for FiniteStochasticLanguage {
    fn number_of_traces(&self) -> usize {
        self.traces.len()
    }

    fn get_trace<'a>(
        &'a self,
        trace_index: usize,
        _result_cache: &'a mut Vec<Activity>,
    ) -> Option<&'a Vec<Activity>> {
        self.traces.keys().nth(trace_index)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::{Fraction, Zero};

    use crate::{IndexTrace, ebi_objects::finite_stochastic_language::FiniteStochasticLanguage};

    #[test]
    fn empty_slang() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let mut slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        slang.normalise();

        assert_eq!(slang.number_of_traces(), 0);
        assert_eq!(
            slang.traces.values().fold(Fraction::zero(), |mut x, y| {
                x += y;
                x
            }),
            Fraction::zero()
        );
    }
}
