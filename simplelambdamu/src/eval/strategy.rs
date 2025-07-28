use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Strategy {
    pub beta_cbn: bool,
    pub use_strv: bool,
    pub use_eta: bool,
    pub use_lam: bool,
    pub use_mu: bool,
}

impl Strategy {
    pub fn new() -> Self {
        Self {
            beta_cbn: false,
            use_strv: false,
            use_eta: false,
            use_lam: false,
            use_mu: false,
        }
    }

    pub fn from(s: &str) -> Result<Self, String> {
        Ok(match s {
            "normalorder" => Self {
                beta_cbn: true,
                use_strv: true,
                use_eta: false,
                use_lam: true,
                use_mu: true,
            },
            "normalorderwitheta" => Self {
                beta_cbn: true,
                use_strv: true,
                use_eta: true,
                use_lam: true,
                use_mu: true,
            },
            "cbv" => Self {
                beta_cbn: false,
                use_strv: true,
                use_eta: false,
                use_lam: false,
                use_mu: false,
            },
            "cbn" => Self {
                beta_cbn: true,
                use_strv: false,
                use_eta: false,
                use_lam: false,
                use_mu: false,
            },
            "cbvwitheta" => Self {
                beta_cbn: false,
                use_strv: true,
                use_eta: true,
                use_lam: false,
                use_mu: false,
            },
            "cbnwitheta" => Self {
                beta_cbn: true,
                use_strv: false,
                use_eta: true,
                use_lam: false,
                use_mu: false,
            },
            _ => return Err(format!("Unknown strategy: {}", s)),
        })
    }

    pub fn name(&self) -> &'static str {
        match self {
            s if s.beta_cbn && s.use_strv && s.use_lam && s.use_mu && !s.use_eta => "normalorder",
            s if s.beta_cbn && s.use_strv && s.use_lam && s.use_mu && s.use_eta => {
                "normalorderwitheta"
            }
            s if !s.beta_cbn && s.use_strv && !s.use_eta => "cbv",
            s if s.beta_cbn && !s.use_strv && !s.use_eta => "cbn",
            s if !s.beta_cbn && s.use_strv && s.use_eta => "cbvwitheta",
            s if s.beta_cbn && !s.use_strv && s.use_eta => "cbnwitheta",
            _ => "custom",
        }
    }
}

impl fmt::Display for Strategy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Default for Strategy {
    fn default() -> Self {
        Self::new()
    }
}
