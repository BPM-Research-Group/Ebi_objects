use crate::{
    EbiObject, Exportable, Importable, NumberOfTraces, traits::importable::{ImporterParameter, ImporterParameterValues}
};
use anyhow::anyhow;
use std::io::BufRead;

#[derive(Clone)]
/// A Python log is an XES log, but without trace attributes.
/// It is represented in memory only; there is no file format; the importer and exporter will always fail.
pub struct EventLogPython {
    pub log: process_mining::EventLog,
}

impl Importable for EventLogPython {
    const IMPORTER_PARAMETERS: &[ImporterParameter] = &[];

    const FILE_FORMAT_SPECIFICATION_LATEX: &str = "A Pm4py log can be imported by Ebi from Python. Ensure to set \\texttt{{return_legacy_log_object=True}} at importing.";

    fn import_as_object(
        _reader: &mut dyn BufRead,
        _parameter_values: &ImporterParameterValues,
    ) -> anyhow::Result<EbiObject> {
        Err(anyhow!("A Python log can only be imported from Python."))
    }

    fn import(
        _reader: &mut dyn BufRead,
        _parameter_values: &ImporterParameterValues,
    ) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        Err(anyhow!("A Python log can only be imported from Python."))
    }
}

impl Exportable for EventLogPython {
    fn export_from_object(_object: EbiObject, _f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Err(anyhow!("A Python log can only be exported to Python."))
    }

    fn export(&self, _f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Err(anyhow!("A Python log can only be exported to Python."))
    }
}

impl NumberOfTraces for EventLogPython {
    fn number_of_traces(&self) -> usize {
        self.log.traces.len()
    }

    fn number_of_events(&self) -> usize {
        self.log.traces.iter().map(|trace| trace.events.len()).sum()
    }
}
