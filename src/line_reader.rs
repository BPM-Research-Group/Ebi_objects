use ebi_activity_key::{Activity, ActivityKey};
use ebi_arithmetic::Fraction;
use ebi_arithmetic::anyhow::{Context, Result, anyhow};
use std::io::BufRead;

pub struct LineReader<'a> {
    reader: &'a mut dyn BufRead,
    line_no: usize,
    line: String,
}

impl<'a> LineReader<'a> {
    pub fn new(reader: &'a mut (dyn BufRead + 'a)) -> Self {
        LineReader::<'a> {
            reader: reader,
            line_no: 0,
            line: String::new(),
        }
    }

    pub fn get_last_line_number(&self) -> usize {
        self.line_no
    }

    pub fn get_last_line(&self) -> &str {
        &self.line
    }

    pub fn next_line_raw(&mut self) -> Result<()> {
        self.line.clear();

        match self.reader.read_line(&mut self.line) {
            Ok(0) => return Err(anyhow!("premature end of file")),
            Ok(_n) => {
                if self.line.ends_with('\n') {
                    self.line.pop();
                    if self.line.ends_with('\r') {
                        self.line.pop();
                    }
                }
                self.line_no += 1;
                return Ok(());
            }
            Err(e) => Err(e.into()),
        }
    }

    pub fn next_line_string(&mut self) -> Result<String> {
        self.next_line()?;
        Ok(self.get_last_line().to_string())
    }

    pub fn next_line_index(&mut self) -> Result<usize> {
        self.next_line()?;
        self.get_last_line()
            .trim()
            .parse::<usize>()
            .with_context(|| {
                format!(
                    "failed to read integer at line {}; found `{}`",
                    self.get_last_line_number(),
                    self.get_last_line()
                )
            })
    }

    pub fn next_line_natural(&mut self) -> Result<u64> {
        self.next_line()?;
        self.get_last_line().trim().parse::<u64>().with_context(|| {
            format!(
                "failed to read integer at line {}; found `{}`",
                self.get_last_line_number(),
                self.get_last_line()
            )
        })
    }

    pub fn next_line_bool(&mut self) -> Result<bool> {
        self.next_line()?;
        self.get_last_line()
            .trim()
            .parse::<bool>()
            .with_context(|| {
                format!(
                    "failed to read boolean at line {}; found `{}`",
                    self.get_last_line_number(),
                    self.get_last_line()
                )
            })
    }

    pub fn next_line_weight(&mut self) -> Result<Fraction> {
        self.next_line()?;
        //attempt to read a rational
        let result = self
            .get_last_line()
            .trim()
            .parse::<Fraction>()
            .with_context(|| {
                format!(
                    "failed to interpret line {} as rational or float; found `{}`",
                    self.get_last_line_number(),
                    self.get_last_line()
                )
            })?;
        return Ok(result);
    }

    pub fn next_line(&mut self) -> Result<()> {
        //read line and unpack
        self.next_line_raw()?;
        while self.get_last_line().trim_start().starts_with('#') {
            self.next_line_raw()?;
        }
        Ok(())
    }

    pub fn next_activity_or_silent(
        &mut self,
        activity_key: &mut ActivityKey,
    ) -> Result<Option<Activity>> {
        let line = self
            .next_line_string()
            .with_context(|| format!("Failed to read activity."))?;

        if line.trim_start().starts_with("label ") {
            let label = line.trim_start()[6..].to_string();
            let activity = activity_key.process_activity(&label);
            Ok(Some(activity))
        } else if line.trim_start().starts_with("multiline label ") {
            let mut label = line.trim_start()[16..].to_string();
            while !label.ends_with("$") && !label.ends_with("$$") {
                label += "\n";
                label += &self.next_line_string().with_context(|| {
                    anyhow!("Was expecting a line ending with `$' to end a multiline label.")
                })?;
            }
            label.pop(); //remove last $
            label = label.replace("$$\n", "$\n"); //un-escape $$ at the end of the line to $

            let activity = activity_key.process_activity(&label);
            Ok(Some(activity))
        } else {
            //silent
            Ok(None)
        }
    }

    pub fn next_activity(&mut self, activity_key: &mut ActivityKey) -> Result<Activity> {
        let mut line = self
            .next_line_string()
            .with_context(|| format!("Failed to read activity."))?;

        //count starting $s
        let ampersands = line.chars().count() - line.trim_start_matches("&").chars().count();
        if ampersands % 2 == 0 {
            //even number of ampersands -> single-line, but half their number
            for _ in 0..ampersands / 2 {
                line.remove(0);
            }
            let activity = activity_key.process_activity(&line);
            Ok(activity)
        } else {
            //odd number of ampersands -> multiline
            line.remove(0);

            let mut label = line.trim_start()[1..].to_string();
            while !label.trim_end().ends_with("$") && !label.trim_end().ends_with("$$") {
                label += "\n";
                label += &self.next_line_string().with_context(|| {
                    anyhow!("Was expecting a line ending with `$' to end a multiline label.")
                })?;
            }
            label.pop(); //remove last $
            label = label.replace("$$\n", "$\n"); //un-escape $$ at the end of the line to $

            let activity = activity_key.process_activity(&label);
            Ok(activity)
        }
    }
}
