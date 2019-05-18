\ProvidesClass{scspaperproc}
\NeedsTeXFormat{LaTeX2e}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[11pt, twoside, onecolumn, letterpaper]{article}

\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\input glyphtounicode
\pdfgentounicode=1
% make sure that the code is copy-pasteable and does not contain ligatures (without it, e.g. ``ff'' or ``ffi'' are typeset as a single unicode character and copy-pasting them yields a [] box character; Lukas Polok 2016-09-08)

\RequirePackage{scsproc}
\usepackage{tikz}

\def\SCSconferenceacro{\textless{}Conference Acronym\textgreater}
\def\SCSsymposiumacro{\textless{}Symposium Acronym\textgreater}
\def\SCSpublicationyear{\textless{}Year\textgreater}
\def\SCSconferencedates{\textless{}Month DD-DD\textgreater}
\def\SCSconferencevenue{\textless{}City, State, Country\textgreater}
\def\SCSlinebreak{\hfil\penalty 100 \hfilneg} % This breaks the line in half, only in case it would be too long to keep it in a single line. If there are problems (overflows or unwanted breaks, you can easily refefine this macro in your tex file to evaluate to either nothing or to a line break ``\\'' or alternatively ``\linebreak'').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The copyright line. This is now assembled from several macros found in the .tex file, in order to enforce form.
\newcommand\copyrighttext{\footnotesize%
\begin{raggedright}%
    \mbox{{\protect\SCSconferenceacro}-{\protect\SCSsymposiumacro}, {\protect\SCSpublicationyear} {\protect\SCSconferencedates}, {\protect\SCSconferencevenue};} {\protect\SCSlinebreak}\mbox{\copyright{\protect\SCSpublicationyear} Society for Modeling \& Simulation International (SCS)}
\end{raggedright}%
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% explanatory comments for creating a correct copyright notice:
% The format of the copyright notice is:
% confAcro-sympAcro, YYYY Month Dates, City, State, Country; ©YYYY Society for Modeling & Simulation International (SCS)
% The notice variables are: confAcro (conference acronym), sympAcro (symposium acronym), YYYY (4
% digit year the conference was held), Month (full name of the month the conference was held), Dates
% (date range of the conference), City (the full name of the city where the conference was held), State
% (standard 2-letter US postal abbreviation of the State where the conference was held) and Country
% (the standard international abbreviation of the name of the country where the conference was
% held). If the conference was held outside of the US, replace State with the appropriate value for the
% country where the conference was held. Replace all variables in both lines of the sample copyright
% notice with the appropriate values for your conference & symposium. Copyright will be held by SCS.
% Accepted work-in-progress and papers will be distributed in the Conference Publications. They will
% also be placed in the ACM Digital Library, as well as other prominent digital libraries, where they will
% remain accessible to thousands of researchers and practitioners worldwide.

%\usepackage{microtype} % improve the text flow a bit (Lukas Polok, 2016-10-05)
%\usepackage[english]{babel} % breaks the *autorefname commands, necessitating redefinition in the main tex. let's leave it simple for now
%\input{ushyphex.tex} % US english hyphenation exceptions (can get them at http://mirrors.ctan.org/info/digests/tugboat/hyphenex/ushyphex.tex); unfortunately this is not a part of all the tex distributions and would cause errors
% word hyphenation (Lukas Polok 2016-09-08)

\newcommand\copyrightnotice{%
\begin{tikzpicture}[remember picture,overlay]
\node[anchor=north,yshift=75pt] at (current page.south) {\mbox{\parbox{\dimexpr\textwidth}{\textit{\copyrighttext}}}};
\end{tikzpicture}%
} % čhanged the anchor from south to north so that the two-line copyright notice keeps the same spacing from the text as a one-line copyright notice (e.g. with HPC it is one line, with TMS/DEVS it jumps to two lines), changed the vertical shift from 40pt to 75pt (Lukas Polok 2016-10-04)

\let\titlelowercase\title
\renewcommand{\title}[1]{\titlelowercase{\uppercase{#1}}}
\let\sectionlowercase\section
\usepackage{suffix}
\renewcommand\section[1]{\sectionlowercase{\uppercase{#1}}}
\WithSuffix\newcommand\section*[1]{\sectionlowercase*{\uppercase{#1}}}
% automatically uppercase the sections and the title (not the subsections though; Lukas Polok 2016-09-07)

\def\algorithmautorefname{Algorithm} % make \autoref work with algorithms
\def\subsectionautorefname{Section}
\def\sectionautorefname{Section}
\def\subsubsectionautorefname{Section}
\def\subsubsubsectionautorefname{Section}
\providecommand{\subfigureautorefname}{\figureautorefname} % Fix to getting autorefs for subfigures right
% fix names of the \autoref targets (Lukas Polok 2016-09-07)

%\addto\extrasenglish{
%\renewcommand*{\sectionautorefname}{Section}
%\renewcommand*{\subsectionautorefname}{Section}
%\renewcommand*{\subsubsectionautorefname}{Section}
%\renewcommand*{\subsubsubsectionautorefname}{Section}
%} % fixes for \autoref when babel is used
% in case babel is enabled above, this needs to be placed in the main tex, after the inclusion of hyperref

%\usepackage{titlesec}
%\titlespacing{\subsubsection}{0pt}{\parskip}{-\parskip}
