#[cfg(test)]
use crate::activity_key::has_activity_key::TestActivityKey;
use crate::{
    ActivityKey, CompressedEventLogXes, EventLog, EventLogXes, HasActivityKey,
    TranslateActivityKey,
    constants::ebi_object::EbiObject,
    traits::{
        exportable::Exportable,
        importable::{Importable, ImporterParameter, ImporterParameterValues, from_string},
    },
};
use anyhow::{Context, Result, anyhow};
use flate2::{Compression, write::GzEncoder};
use std::io::{BufRead, Write};

pub struct CompressedEventLog {
    pub log: EventLog,
}

impl Importable for CompressedEventLog {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        CompressedEventLogXes::FILE_FORMAT_SPECIFICATION_LATEX;

    const IMPORTER_PARAMETERS: &[ImporterParameter] = EventLogXes::IMPORTER_PARAMETERS;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        let log = Self::import(reader, parameter_values)?;
        Ok(EbiObject::EventLog(log.log))
    }

    fn import(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let log = CompressedEventLogXes::import(reader, parameter_values)?;
        Ok(log.into())
    }
}
from_string!(CompressedEventLog);

impl Exportable for CompressedEventLog {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => {
                let xes: Self = log.into();
                xes.export(f)
            }
            EbiObject::EventLogTraceAttributes(log) => {
                let xes: Self = log.into();
                xes.export(f)
            }
            EbiObject::EventLogXes(log) => {
                let xes: Self = log.into();
                xes.export(f)
            }
            EbiObject::EventLogCsv(log) => {
                let xes: Self = log
                    .try_into()
                    .with_context(|| anyhow!("Cannot transform csv to xes."))?;
                xes.export(f)
            }
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        self.log.export(&mut writer)
    }
}

impl HasActivityKey for CompressedEventLog {
    fn activity_key(&self) -> &ActivityKey {
        self.log.activity_key()
    }

    fn activity_key_mut(&mut self) -> &mut ActivityKey {
        self.log.activity_key_mut()
    }
}

impl TranslateActivityKey for CompressedEventLog {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        self.log.translate_using_activity_key(to_activity_key)
    }
}

#[cfg(test)]
impl TestActivityKey for CompressedEventLog {
    fn test_activity_key(&self) {
        self.log.test_activity_key();
    }
}
