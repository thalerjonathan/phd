% class for the Journal of Artificial Societies and Social Simulation v.0.5, 2015-03-23
% Author Giangiacomo Bravo <giangiacomo.bravo@lnu.se>
\ProvidesClass{JASSS}


\NeedsTeXFormat{LaTeX2e}
\LoadClass[a4paper,10pt]{article}

% Font used: Source Sans Pro
% included in Tex Live
% Location at CTAN: http://www.ctan.org/tex-archive/fonts/sourcesanspro/
% See also: http://www.tug.dk/FontCatalogue/sourcesanspro/
\RequirePackage[default]{sourcesanspro}
\RequirePackage[T1]{fontenc}

\RequirePackage{graphicx,xcolor, booktabs,amsmath, ifthen}	
\RequirePackage[left = 2.5cm, right = 2.5cm, top = 2cm, bottom = 3cm]{geometry}	

\RequirePackage[noblocks]{authblk}
	\renewcommand\Authfont{\bfseries\large}
	\renewcommand\Affilfont{\normalfont\small\itshape}
	\renewcommand\Authands{, }

\RequirePackage{endnotes}
\renewcommand{\footnotesize}{\normalsize}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% new environments and commands 

\newenvironment{itemize*}{\begin{itemize}}{\end{itemize}\par\parano{}}
\newenvironment{enumerate*}{\begin{enumerate}}{\end{enumerate}\par\parano{}}
\newenvironment{description*}{\begin{description}}{\end{description}\par\parano{}}

\newcommand{\doinum}[1]{\def \@doinum{#1}}
\doinum{10.18564/jasss.xxxx}

\newcommand{\email}[1]{\def \@email{#1}}
\email{}

\newcommand{\volume}[1]{\def \@volume{#1}}
\volume{xx}

\newcommand{\issue}[1]{\def \@issue{#1}}
\issue{x}

\newcommand{\article}[1]{\def \@article{#1}}
\article{x}

\newcommand{\pubyear}[1]{\def \@pubyear{#1}}
\pubyear{20xx}

\newcommand{\received}[1]{\def \@received{#1}}
\received{dd-mmm-yyyy}

\newcommand{\accepted}[1]{\def \@accepted{#1}}
\accepted{dd-mmm-yyyy}

\newcommand{\published}[1]{\def \@published{#1}}
\published{dd-mmm-yyyy}

\newcommand{\reviewcopy}[1]{\def \@reviewcopy{#1}}
\reviewcopy{true}

\newcommand{\JASSScolor}{blue!60!gray}

\let\oldtabular\tabular 
\renewcommand{\tabular}{\small \oldtabular}

\date{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Abstract and keywords

\renewenvironment{abstract}[1]{\vspace{0.5cm} \textcolor{\JASSScolor}{\textbf{Abstract:}} #1}{\bigskip}
\newenvironment{keywords}[1]{\textcolor{\JASSScolor}{\textbf{Keywords:}} #1}{\vspace{0.5cm}\color{\JASSScolor} \hrule}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Headers and footers

\RequirePackage{fancyhdr}
	\pagestyle{fancy}
	\lhead{}
	\chead{}
	\rhead{}
	\lfoot{
		\ifthenelse{\equal{\@reviewcopy}{false}}
			{	
				\small \textit{JASSS}, \@volume(\@issue) \@article, \@pubyear
			}
			{
				\textit{Anonymised review copy}
			}
	}
	\cfoot{\footnotesize
		\ifthenelse{\equal{\@reviewcopy}{false}}
			{		
				\small http://jasss.soc.surrey.ac.uk/\@volume/\@issue/\@article.html
			}
			{}
	}
	\rfoot{Doi: \@doinum}
	\renewcommand{\headrulewidth}{0pt}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Title section

\renewcommand{\maketitle}{%
	\thispagestyle{fancy}
	{
	%\color{\JASSScolor}
	\includegraphics[width=1\textwidth]{bar} %\hrule	
	
	%\vspace{0.5cm}
	\begin{minipage}[b]{0.25\textwidth}
		\includegraphics[width=1\textwidth]{JASSS}
	\end{minipage}
	\hfill
	\begin{minipage}{0.74\textwidth}
		\LARGE \textbf{\@title} \par	\bigskip	
		\ifthenelse{\equal{\@reviewcopy}{false}}
		{		
			\@author \par 
			\normalfont\small *Correspondence should be addressed to {\it\@email} \par\medskip
			\textit{Journal of Artificial Societies and Social Simulation}  \@volume(\@issue) \@article, (\@pubyear). Doi: \@doinum\par Url: http://jasss.soc.surrey.ac.uk/\@volume/\@issue/\@article.html\par\medskip
			Received: \@received \hspace{0.5cm} Accepted: \@accepted \hspace{0.5cm} Published: \@published
		}
		{
			\normalsize \textit{[Anonymised review copy]}
		}
	\end{minipage}		
	%\vspace{0.3cm}
	
	\includegraphics[width=1\textwidth]{bar} %\hrule 
	}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Paragraphs and paragraph counter

\setlength{\parindent}{0pt}
\setlength{\parskip}{1ex plus 0.5ex minus 0.5ex}

\makeatletter
\def\@seccntformat#1{\protect\makebox[0pt][r]{\csname
the#1\endcsname\quad}}


%% The code below was extracted from the parano package (parano.sty)
%% Originak file: parano.dtx Copyright (c) 2003, CV Radhakrishnan
%%       River Valley Technologies, Trivandrum, India
%%       <cvr@river-valley.org>

\newcounter{parano}
%\@addtoreset{parano}{section}

\def\prepara{}
\def\postpara{}
\newdimen\paranoboxwidth
\paranoboxwidth=4em
\newdimen\parwidth
%\renewcommand\theparano{\thesection.\arabic{parano}}
\renewcommand\theparano{\@arabic\c@section.\arabic{parano}}
                                                              
\newif\ifparano\paranofalse

\def\parano{\global\paranotrue\@ifnextchar[{\@@parano}{\@parano}}
\AtBeginDocument{\parwidth\parindent}
\long\def\@parano{%%
%\typeout{PARINDENT = \the\parindent----PARWIDTH =  \the\parwidth}
   \everypar{\parindent0pt\refstepcounter{parano}%
   \hbox to \paranoboxwidth{\prepara\theparano\hfill}\postpara}}

\long\def\@@parano[#1]{%
   \setcounter{parano}{#1}%
   \addtocounter{parano}{-1}%
   \everypar{\parindent0pt%
   \refstepcounter{parano}%
   \hbox to \paranoboxwidth{\prepara\theparano\hfill}\postpara}}

\def\endparano{\everypar{\parindent\parwidth}
         \global\paranofalse%
         \parindent\parwidth%
%\typeout{PARINDENT = \the\parindent::::PARWIDTH =  \the\parwidth}
}

\def\@xsect#1{%
  \@tempskipa #1\relax
  \ifdim \@tempskipa>\z@
    \par \nobreak
    \vskip \@tempskipa
    \@afterheading
  \else
    \@nobreakfalse
    \global\@noskipsectrue
    \everypar{%
      \if@noskipsec
        \global\@noskipsecfalse
       {\setbox\z@\lastbox}%
        \clubpenalty\@M
        \begingroup \@svsechd \endgroup
        \unskip
        \@tempskipa #1\relax
        \hskip -\@tempskipa
      \else
        \clubpenalty \@clubpenalty
        \everypar{}%
      \fi}%
  \fi
  \ifparano\@parano\fi\ignorespaces}

\def\@startsection#1#2#3#4#5#6{\everypar{}%
  \if@noskipsec \leavevmode \fi
  \par
  \@tempskipa #4\relax
  \@afterindenttrue
  \ifdim \@tempskipa <\z@
    \@tempskipa -\@tempskipa \@afterindentfalse
  \fi
    \everypar{}%
    \addpenalty\@secpenalty\addvspace{\@tempskipa}
  \@ifstar
    {\@ssect{#3}{#4}{#5}{#6}}%
    {\@dblarg{\@sect{#1}{#2}{#3}{#4}{#5}{#6}}}}

%%
%% End of code estracted from 'parano.sty'
%%

% from http://tex.stackexchange.com/questions/215304/how-to-number-on-the-left-margin-paragraphs-instead-of-lines
\renewcommand{\@parano}{%
  \everypar{\setbox0\lastbox% Fixes first paragraph indent problem
    \refstepcounter{parano}%
    \hbox to \paranoboxwidth{\color{\JASSScolor}\bf\prepara\theparano\paranospace}\postpara}}

\def\@@parano[#1]{\setcounter{parano}{#1}\addtocounter{parano}{-1}%
   \@parano}% To avoid code duplication
   
\makeatother

\renewcommand{\prepara}{\hss}
\renewcommand{\postpara}{\hspace{\parindent}\ignorespaces}
\newcommand{\paranospace}{\quad\hfill}
\setlength{\paranoboxwidth}{0pt}
\renewcommand\thesection{\setcounter{parano}{0}}
\renewcommand\thesubsection{}
\renewcommand\thesubsubsection{}

\setcounter{secnumdepth}{1}

\RequirePackage{sectsty}
\sectionfont{\color{\JASSScolor}} 
\subsectionfont{\color{\JASSScolor}} 
\subsubsectionfont{\color{\JASSScolor}} 

