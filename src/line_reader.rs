use ebi_activity_key::{Activity, ActivityKey};
use ebi_arithmetic::Fraction;
use ebi_arithmetic::anyhow::{Context, Result, anyhow};
use std::fmt;
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
        } else if line.trim_start() == "multiline label" {
            let mut last_line = self
                .next_line_string()
                .with_context(|| anyhow!("Starting with multiline activity."))?;
            let mut label = String::new();
            let line_number = self.get_last_line_number();

            while last_line != "multiline$" {
                if last_line.ends_with("$") {
                    last_line.pop();
                }
                label += &last_line;
                label += "\n";
                last_line = self.next_line_string().with_context(|| {
                    anyhow!("Was expecting a line `multiline$' to end a multiline label that started on line {}.", line_number)
                })?;
            }

            label.pop(); //remove the last \n
            let activity = activity_key.process_activity(&label);
            return Ok(Some(activity));
        } else {
            //silent
            Ok(None)
        }
    }

    pub fn next_activity(&mut self, activity_key: &mut ActivityKey) -> Result<Activity> {
        let l = self
            .next_line_string()
            .with_context(|| format!("Failed to read activity."))?;
        let mut line = l.as_str();
        let line_number = self.get_last_line_number();

        //decide on multiline
        if line == "$multiline" {
            //multiline

            let mut last_line = self
                .next_line_string()
                .with_context(|| anyhow!("Starting with multiline activity."))?;
            let mut label = String::new();

            while last_line != "multiline$" {
                if last_line.ends_with("$") {
                    last_line.pop();
                }
                label += &last_line;
                label += "\n";
                last_line = self.next_line_string().with_context(|| {
                    anyhow!("Was expecting a line `multiline$' to end a multiline label that started on line {}.", line_number)
                })?;
            }

            label.pop(); //remove the last \n
            let activity = activity_key.process_activity(&label);
            return Ok(activity);
        }

        if line.starts_with("$") {
            line = &line[1..];
        }

        //single line
        let activity = activity_key.process_activity(line);
        Ok(activity)
    }

    pub fn write_multiline_activity(
        f: &mut fmt::Formatter,
        activity: &Activity,
        activity_key: &ActivityKey,
    ) -> fmt::Result {
        let label = activity_key.deprocess_activity(activity);
        if !label.contains("\n") {
            if label.starts_with("$") {
                writeln!(f, "${}", label)
            } else {
                writeln!(f, "{}", label)
            }
        } else {
            writeln!(f, "$multiline")?;
            write!(f, "{}", (label.to_owned() + "\n").replace("$\n", "$$\n"))?;
            writeln!(f, "multiline$")
        }
    }

    pub fn write_activity_or_silent(
        f: &mut std::fmt::Formatter<'_>,
        activity: Option<Activity>,
        activity_key: &ActivityKey,
    ) -> std::fmt::Result {
        if let Some(activity) = activity {
            let activity_label = activity_key.get_activity_label(&activity);
            if activity_label.contains("\n") {
                let activity_label = (activity_label.to_owned() + "\n").replace("$\n", "$$\n");
                writeln!(f, "multiline label\n{}multiline$", activity_label)
            } else {
                writeln!(f, "label {}", activity_label)
            }
        } else {
            writeln!(f, "silent")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Exportable, FiniteStochasticLanguage, FiniteStochasticPartiallyOrderedLanguage};
    use std::fs;

    #[test]
    fn activity_read_write_slang() {
        let fin = fs::read_to_string("testfiles/multiline.slang").unwrap();
        let mut slang = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let act = slang
            .activity_key
            .process_activity("a\nbcde\nfghi$$\nmultiline$");
        let act2 = slang.activity_key.process_activity("$multiline");
        println!("{:?}", slang.activity_key);
        assert!(slang.activity_key.get_id_from_activity(act) < 2);
        assert!(slang.activity_key.get_id_from_activity(act2) < 2);

        let mut fout: Vec<u8> = vec![];
        slang.export(&mut fout).unwrap();
        assert_eq!(String::from_utf8_lossy(&fout), fin);
    }

    #[test]
    fn activity_read_write_psolang() {
        let fin = fs::read_to_string("testfiles/model.sbpmn.spolang").unwrap();
        let mut slang = fin
            .parse::<FiniteStochasticPartiallyOrderedLanguage>()
            .unwrap();

        let act = slang.activity_key.process_activity("Check easy claim
(5 min)");
        assert!(slang.activity_key.get_id_from_activity(act) < 3);

        let mut fout: Vec<u8> = vec![];
        slang.export(&mut fout).unwrap();
        assert_eq!(String::from_utf8_lossy(&fout), fin);
    }
}
