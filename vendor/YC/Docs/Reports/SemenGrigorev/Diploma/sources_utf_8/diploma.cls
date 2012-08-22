\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{diploma}[2006/05/22 LaTeX Diploma class]
\LoadClass[a4paper,14pt]{article}
\usepackage[utf8]{inputenc}
\usepackage[english,russian]{babel}

\newcommand{\university}{\renewcommand{\@university}}
\newcommand{\@university}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\facility}{\renewcommand{\@facility}}
\newcommand{\@facility}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\chair}{\renewcommand{\@chair}}
\newcommand{\@chair}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\position}{\renewcommand{\@position}}
\newcommand{\@position}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\group}{\renewcommand{\@group}}
\newcommand{\@group}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\leader}{\renewcommand{\@leader}}
\newcommand{\@leader}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\leaderPosition}{\renewcommand{\@leaderPosition}}
\newcommand{\@leaderPosition}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\critic}{\renewcommand{\@critic}}
\newcommand{\@critic}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\criticPosition}{\renewcommand{\@criticPosition}}
\newcommand{\@criticPosition}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\chairLeader}{\renewcommand{\@chairLeader}}
\newcommand{\@chairLeader}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\chairLeaderPosition}{\renewcommand{\@chairLeaderPosition}}
\newcommand{\@chairLeaderPosition}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\yr}{\renewcommand{\@yr}}
\newcommand{\@yr}{\ClassWarning{diploma}{No parameter given}\@eha}
\newcommand{\city}{\renewcommand{\@city}}
\newcommand{\@city}{\ClassWarning{diploma}{No parameter given}\@eha}

\renewcommand\maketitle{\begin{titlepage}%
  \begin{center}
    {\Large \@university \\ \@facility \\}
    \vskip 1.5em%
    {\large \@chair \\}
    \vskip 2em%
    {\Huge \@title \\}
    \vskip 1.5em%
    {\large Выпускная квалификационная работа бакалавра  \\ \@position\ \@group\ группы\\}
    {\Large\it\@author\\}
    \vskip 3em%
    {\normalsize
    \begin{tabular}{l p{0.1cm} c p{0.1cm} l}
    Научный руководитель && .................. && \@leaderPosition\ \@leader \\
    && /подпись/ && \\
    Рецензент && .................. && \@criticPosition\ \@critic \\
    && /подпись/ && \\
    ``Допустить к защите'' && .................. && \@chairLeaderPosition\ \@chairLeader \\
    заведующий кафедрой && /подпись/ && \\
    \end{tabular}
    }
    \vskip 1em%
    {\large \@city\\ \@yr}%
  \end{center}\par
  \end{titlepage}%
}
