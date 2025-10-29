use anyhow::Result;
use flate2::{Compression, write::GzEncoder};
use std::io::{BufRead, Write};

use crate::{
    ActivityKey, CompressedEventLogXes, EventLog, EventLogXes, HasActivityKey,
    TranslateActivityKey,
    constants::ebi_object::EbiObject,
    traits::{
        exportable::Exportable,
        importable::{Importable, ImporterParameter, ImporterParameterValues, from_string},
    },
};

pub struct CompressedEventLog {
    pub log: EventLog,
}

impl Importable for CompressedEventLog {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        CompressedEventLogXes::FILE_FORMAT_SPECIFICATION_LATEX;

    const IMPORTER_PARAMETERS: &[ImporterParameter] = EventLogXes::IMPORTER_PARAMETERS;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: ImporterParameterValues,
    ) -> Result<EbiObject> {
        let log = Self::import(reader, parameter_values)?;
        Ok(EbiObject::EventLog(log.log))
    }

    fn import(
        reader: &mut dyn BufRead,
        parameter_values: ImporterParameterValues,
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
            EbiObject::EventLog(log) => Self::export(&Self { log }, f),
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
