%%
%% This is file `IEEEconf.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% IEEEconf.dtx  (with options: `class')
%% 
%% This is a generated file.
%% 
%% Copyright (C) 2009 by Scott Pakin <scott+iecnf@pakin.org>
%% 
%% This file may be distributed and/or modified under the conditions of
%% the LaTeX Project Public License, either version 1.3c of this license
%% or (at your option) any later version.  The latest version of this
%% license is in:
%% 
%%    http://www.latex-project.org/lppl.txt
%% 
%% and version 1.3c or later is part of all distributions of LaTeX version
%% 2006/05/20 or later.
%% 
\NeedsTeXFormat{LaTeX2e}[1999/12/01]
\ProvidesClass{IEEEconf}
    [2009/04/05 v1.4 IEEE Computer Society 8.5x11" conference proceedings]
\def\ieee@column@spec{twocolumn}
\DeclareOption{onecolumn}{\def\ieee@column@spec{onecolumn}}
\newif\if@ieee@correct@geometry@
\@ieee@correct@geometry@true
\DeclareOption{latex8}{\@ieee@correct@geometry@false}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\PassOptionsToClass{\ieee@column@spec,10pt}{article}
\LoadClass{article}
\let\ieee@old@maketitle=\maketitle
\renewcommand{\maketitle}{%
  \ieee@old@maketitle
  \thispagestyle{empty}%
}
\pagestyle{empty}
\setlength{\parindent}{0.25in}
\if@ieee@correct@geometry@
  \RequirePackage[%
    noheadfoot,
    textwidth=6.5in,
    textheight=8.875in,
    tmargin=1in
  ]{geometry}
  \setlength{\columnsep}{0.375in}
\else
  \setlength{\textheight}{8.875in}
  \setlength{\textwidth}{6.875in}
  \setlength{\columnsep}{0.3125in}
  \setlength{\topmargin}{0in}
  \setlength{\headheight}{0in}
  \setlength{\headsep}{0in}
  \setlength{\parindent}{1pc}
  \setlength{\oddsidemargin}{-.304in}
  \setlength{\evensidemargin}{-.304in}
\fi
\RequirePackage{mathptmx}
\RequirePackage{helvet}
\RequirePackage{courier}
\newcommand{\ieee@times@xiv@bold}{%
  \rmfamily\bfseries\upshape\fontsize{14}{16}\selectfont}
\newcommand{\ieee@times@xii@bold}{%
  \rmfamily\bfseries\upshape\fontsize{12}{14}\selectfont}
\newcommand{\ieee@times@xi@bold}{%
  \rmfamily\bfseries\upshape\fontsize{11}{13}\selectfont}
\newcommand{\ieee@times@x@bold}{%
  \rmfamily\bfseries\upshape\fontsize{10}{12}\selectfont}
\newcommand{\ieee@times@xii@roman}{%
  \rmfamily\mdseries\upshape\fontsize{12}{14}\selectfont}
\newcommand{\ieee@times@xii@italic}{%
  \rmfamily\mdseries\itshape\fontsize{12}{14}\selectfont}
\newcommand{\ieee@times@ix@roman}{%
  \rmfamily\mdseries\upshape\fontsize{9}{11}\selectfont}
\newcommand{\ieee@helv@ix@roman}{%
  \sffamily\mdseries\upshape\fontsize{9}{11}\selectfont}
\newcommand{\ieee@helv@x@bold}{%
  \sffamily\bfseries\upshape\fontsize{10}{12}\selectfont}
\RequirePackage{array}
\newlength{\ieee@author@width}
\def\and{%
  \end{tabular}%
  \hskip 1em \@plus 0.17fil%
  \rule{0pt}{1.5\baselineskip}%
  \begin{tabular}[t]{@{}>{\centering}p{\ieee@author@width}@{}}%
}
\def\@maketitle{%
  \newpage
  \null
  \vskip 0.375in%
  \begin{center}%
    \let\footnote=\thanks
    {\ieee@times@xiv@bold\@title\par}%
    \vskip 24pt
    \ieee@times@xii@roman
    \settowidth{\ieee@author@width}{%
      \let\and=\crcr
      \let\thanks=\@gobble
      \let\footnote=\@gobble
      \begin{tabular}[t]{@{}c@{}}%
        \@author
      \end{tabular}%
    }%
    \begin{tabular}[t]{@{}>{\hfil}p{\ieee@author@width}<{\hfil}@{}}%
      \@author
    \end{tabular}%
  \end{center}%
  \if@ieee@correct@geometry@
    \vskip -\baselineskip
    \vskip 24pt%
  \else
    \vspace*{0.5em}%
    \vspace*{12pt}%
  \fi
}
\newenvironment{affiliation}{%
  \ieee@times@xii@italic
  \if@ieee@correct@geometry@
  \else
    \rule[10pt]{0pt}{0.5em}%
  \fi
  \begin{tabular}[t]{c}%
}{%
  \end{tabular}%
}
\newcommand*{\email}[1]{%
  {\ieee@times@xii@italic#1}}
\if@titlepage
  \renewenvironment{abstract}{%
      \titlepage
      \null\vfil
      \@beginparpenalty\@lowpenalty
      \begin{center}%
        \bfseries \abstractname
        \@endparpenalty\@M
      \end{center}%
      \par}%
     {\par\vfil\null\endtitlepage}
\else
  \renewenvironment{abstract}{%
      \begin{center}%
        \ieee@times@xii@bold \abstractname
      \end{center}%
      \par
      \itshape}%
      {\vspace*{\baselineskip}}%
\fi
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{\ieee@helv@x@bold #1. #2}%
  \ifdim \wd\@tempboxa >\hsize
    \ieee@helv@x@bold #1. #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\DeclareRobustCommand{\callout}[1]{%
  {\ieee@helv@ix@roman#1}%
}
\RequirePackage{titlesec}
\titleformat{\section}{\ieee@times@xii@bold}{\thesection.}{0.5em}%
  {}\relax
\titlespacing{\section}{0pt}{12pt}{12pt}
\titleformat{\subsection}{\ieee@times@xi@bold}{\thesubsection.}%
  {0.5em}{}\relax
\titlespacing{\subsection}{0pt}{11pt}{11pt}
\titleformat{\subsubsection}[runin]{\ieee@times@x@bold}%
  {\thesubsubsection.}{0.5em}{}[.]
\titlespacing{\subsubsection}{0pt}{10pt}{0.5em}
\renewenvironment{thebibliography}[1]{%
  \section*{\refname}%
  \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}%
  \begin{ieee@thebibitemlist}{#1}%
}{%
  \end{ieee@thebibitemlist}%
}
\newenvironment{ieee@thebibitemlist}[1]
     {\list{\@biblabel{\@arabic\c@enumiv}}%
           {\ieee@times@ix@roman
            \setlength{\itemsep}{0pt}%
            \setlength{\parsep}{0pt}%
            \settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}%
            \extrareflistcode}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\AtBeginDocument{%
  \@ifpackageloaded{tocbibind}{%
    \let\thebibitemlist=\ieee@thebibitemlist
    \let\endthebibitemlist=\endieee@thebibitemlist
  }%
  {}%
}
\newcommand{\ieee@pre@bibitem@key}{IEEEconf}
\newcommand{\ieee@pre@bibitem@cmd}{}
\newcommand{\dobeforekey}[2]{%
  \def\ieee@pre@bibitem@key{#1}%
  \def\ieee@pre@bibitem@cmd{#2}%
}
\let\ieee@old@bibitem=\bibitem
\renewcommand{\bibitem}[2][]{%
  \def\ieee@biblabel{#1}%
  \def\ieee@bibkey{#2}%
  \ifx\ieee@bibkey\ieee@pre@bibitem@key
    \ieee@pre@bibitem@cmd
  \fi
  \ifx\ieee@biblabel\@empty
    \def\next{\ieee@old@bibitem{#2}}%
  \else
    \def\next{\ieee@old@bibitem[#1]{#2}}%
  \fi
  \next
}
\newcommand{\extrareflistcode}{}
\endinput
%%
%% End of file `IEEEconf.cls'.
