use anyhow::Result;
use flate2::{Compression, bufread::GzDecoder, write::GzEncoder};
use std::io::{BufRead, BufReader, Write};

use crate::{
    constants::ebi_object::EbiObject,
    ebi_objects::event_log::EventLog,
    traits::{exportable::Exportable, importable::Importable},
};

pub const FORMAT_SPECIFICATION: &str = "A compressed event log is a gzipped event log file in the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}.
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.";

pub struct CompressedEventLog {
    pub log: EventLog,
}

impl Importable for CompressedEventLog {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLog(Self::import(reader)?.log))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let dec = GzDecoder::new(reader);
        let mut reader2 = BufReader::new(dec);
        let log = EventLog::import(&mut reader2)?;
        Ok(Self { log: log })
    }
}

impl Exportable for CompressedEventLog {
    fn export_from_object(object: EbiObject, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiObject::EventLog(log) => Self::export(&Self { log: log }, f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        let mut writer = GzEncoder::new(f, Compression::best());
        self.log.export(&mut writer)
    }
}
