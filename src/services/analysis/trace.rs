//! Optional tracing for the incremental analysis path.
//!
//! The goal here is targeted observability without sprinkling ad hoc
//! `eprintln!` calls throughout the analysis stack. Tracing is off by default
//! and can be enabled selectively by category.

use std::collections::HashSet;
use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnalysisTraceCategory {
    Query,
    Pipeline,
    Program,
    Hover,
    Session,
}

impl fmt::Display for AnalysisTraceCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = match self {
            Self::Query => "query",
            Self::Pipeline => "pipeline",
            Self::Program => "program",
            Self::Hover => "hover",
            Self::Session => "session",
        };
        f.write_str(label)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisTraceEvent {
    pub category: AnalysisTraceCategory,
    pub message: String,
}

pub trait AnalysisTraceSink: Send + Sync {
    fn emit(&self, event: &AnalysisTraceEvent);
}

#[derive(Default)]
struct StderrTraceSink;

impl AnalysisTraceSink for StderrTraceSink {
    fn emit(&self, event: &AnalysisTraceEvent) {
        eprintln!("[analysis:{}] {}", event.category, event.message);
    }
}

#[derive(Clone, Default)]
pub struct AnalysisTracer {
    enabled: HashSet<AnalysisTraceCategory>,
    sink: Option<Arc<dyn AnalysisTraceSink>>,
}

impl AnalysisTracer {
    pub fn off() -> Self {
        Self::default()
    }

    pub fn stderr<I>(enabled: I) -> Self
    where
        I: IntoIterator<Item = AnalysisTraceCategory>,
    {
        Self::with_sink(enabled, Arc::new(StderrTraceSink))
    }

    pub fn with_sink<I>(enabled: I, sink: Arc<dyn AnalysisTraceSink>) -> Self
    where
        I: IntoIterator<Item = AnalysisTraceCategory>,
    {
        Self {
            enabled: enabled.into_iter().collect(),
            sink: Some(sink),
        }
    }

    pub fn is_enabled(&self, category: AnalysisTraceCategory) -> bool {
        self.sink.is_some() && self.enabled.contains(&category)
    }

    pub fn emit(&self, category: AnalysisTraceCategory, message: impl Into<String>) {
        if !self.is_enabled(category) {
            return;
        }
        let Some(sink) = &self.sink else {
            return;
        };
        sink.emit(&AnalysisTraceEvent {
            category,
            message: message.into(),
        });
    }
}
