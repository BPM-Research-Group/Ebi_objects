use anyhow::Result;
use flate2::{Compression, bufread::GzDecoder, write::GzEncoder};
use std::io::{BufRead, BufReader, Write};

use crate::{
    EventLogTraceAttributes, HasActivityKey, TranslateActivityKey,
    constants::ebi_object::EbiObject,
    traits::{exportable::Exportable, importable::Importable},
};

pub const FORMAT_SPECIFICATION: &str = "A compressed event log is a gzipped event log file in the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}.
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.";

pub struct CompressedEventLog {
    pub log: EventLogTraceAttributes,
}

impl Importable for CompressedEventLog {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLogTraceAttributes(
            Self::import(reader)?.log,
        ))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let dec = GzDecoder::new(reader);
        let mut reader2 = BufReader::new(dec);
        let log = EventLogTraceAttributes::import(&mut reader2)?;
        Ok(Self { log: log })
    }
}

impl Exportable for CompressedEventLog {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLogTraceAttributes(log) => Self::export(&Self { log: log }, f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        self.log.export(&mut writer)
    }
}

impl HasActivityKey for CompressedEventLog {
    fn activity_key(&self) -> &crate::ActivityKey {
        &self.log.activity_key
    }

    fn activity_key_mut(&mut self) -> &mut crate::ActivityKey {
        &mut self.log.activity_key
    }
}

impl TranslateActivityKey for CompressedEventLog {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut crate::ActivityKey) {
        self.log.translate_using_activity_key(to_activity_key);
    }
}
