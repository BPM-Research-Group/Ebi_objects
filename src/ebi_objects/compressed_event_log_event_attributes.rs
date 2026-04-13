use crate::{
    CompressedEventLogXes, EventLogXes, HasActivityKey, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    ebi_objects::event_log_event_attributes::EventLogEventAttributes,
    traits::{
        exportable::Exportable,
        importable::{Importable, ImporterParameter, ImporterParameterValues, from_string},
    },
};
#[cfg(any(test, feature = "testactivities"))]
use ebi_activity_key::TestActivityKey;
use ebi_arithmetic::anyhow::{Error, Result};
use flate2::{Compression, write::GzEncoder};
use std::io::{BufRead, Write};

pub struct CompressedEventLogEventAttributes {
    pub log: EventLogEventAttributes,
}

impl Importable for CompressedEventLogEventAttributes {
    const FILE_FORMAT_SPECIFICATION_LATEX: &str =
        CompressedEventLogXes::FILE_FORMAT_SPECIFICATION_LATEX;

    const IMPORTER_PARAMETERS: &[ImporterParameter] = EventLogXes::IMPORTER_PARAMETERS;

    fn import_as_object(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiObject> {
        let log = Self::import(reader, parameter_values)?;
        Ok(EbiObject::EventLogEventAttributes(log.log))
    }

    fn import(reader: &mut dyn BufRead, parameter_values: &ImporterParameterValues) -> Result<Self>
    where
        Self: Sized,
    {
        let log = CompressedEventLogXes::import(reader, parameter_values)?;
        Ok(log.into())
    }
}
from_string!(CompressedEventLogEventAttributes);

impl Exportable for CompressedEventLogEventAttributes {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLogEventAttributes(log) => Self::export(&Self { log }, f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        self.log.export(&mut writer)
    }
}

impl HasActivityKey for CompressedEventLogEventAttributes {
    fn activity_key(&self) -> &crate::ActivityKey {
        self.log.activity_key()
    }

    fn activity_key_mut(&mut self) -> &mut crate::ActivityKey {
        self.log.activity_key_mut()
    }
}

impl TranslateActivityKey for CompressedEventLogEventAttributes {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut crate::ActivityKey) {
        self.log.translate_using_activity_key(to_activity_key)
    }
}

#[cfg(any(test, feature = "testactivities"))]
impl TestActivityKey for CompressedEventLogEventAttributes {
    fn test_activity_key(&self) {
        self.log.test_activity_key();
    }
}
