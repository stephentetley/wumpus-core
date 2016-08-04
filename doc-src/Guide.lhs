\documentclass{article}

%include polycode.fmt
\usepackage{comment}
\usepackage{amssymb}
\usepackage{alltt}
\usepackage[dvips]{graphicx}

\newcommand{\wumpuscore}{\texttt{wumpus-core} }

\begin{document}

\title{\wumpuscore Guide}
\author{Stephen Tetley}
\maketitle

%-----------------------------------------------------------------




%-----------------------------------------------------------------
\section{About \wumpuscore}
%-----------------------------------------------------------------

This guide was last updated for \wumpuscore version 0.50.0.

\wumpuscore is a Haskell library for generating static 2D vector 
pictures. It is written with portability as a priority, so it has 
no dependencies on foreign C libraries. Output to PostScript and 
SVG (Scalable Vector Graphics) is supported. 

\wumpuscore is rather primitive, the basic drawing objects are 
paths and text labels. A two additional libraries 
\texttt{wumpus-basic} and \texttt{wumpus-drawing} build on 
\wumpuscore adding significant capabilities but they are 
experimental and the APIs they present are unfortunately a long 
way from stable - currently they should be considered a 
\emph{technology preview} not ready for general use.

Although \wumpuscore is heavily inspired by PostScript it avoids 
PostScript's notion of an (implicit) current point and the 
movements \texttt{lineto}, \texttt{moveto} etc., instead 
\wumpuscore aims for a more \emph{coordinate free} style.


%-----------------------------------------------------------------
\section{Exposed modules}
%-----------------------------------------------------------------

\wumpuscore exports the following modules:

\begin{description}
\item[\texttt{Wumpus.Core.}]
Top-level \emph{shim} module to import all the exposed modules. 
Some internal data types are also exported as opaque signatures - 
the implementation is hidden, but the type name is exposed so it 
can be used in the type signatures of \emph{userland} functions. 
Typically, where these data types need to be \emph{instantiated},
smart constructors are provided.

\item[\texttt{Wumpus.Core.AffineTrans.}]
The standard affine transformations (scaling, rotation, 
translation) implemented as type classes, with a set of derived 
operations - reflections about the X or Y axes, rotations through
common angles. 

\item[\texttt{Wumpus.Core.BoundingBox.}]
A data type representing a bounding box and operations on it. 
Bounding boxes are necessary for EPS output, they also support the 
definition of \emph{picture composition} operators, e.g. aligning 
composite pictures horizontally, vertically, etc.

\item[\texttt{Wumpus.Core.Colour.}]
A single colour type \texttt{RGBi} is supported. This type defines 
colour as a triple of integers (Word8) - black is (0, 0, 0); white 
is (255, 255, 255). Some named colours are defined, although they 
are hidden by the top level shim module to avoid name clashes with
libraries providing more extensive lists of colours. 
\texttt{Wumpus.Core.Colour} can be imported directly if its
elementary set of named colours is required.

\item[\texttt{Wumpus.Core.FontSize.}]
Various calculations for font size measurements. \wumpuscore has 
only approximate handling of font / character size as it does not 
interpret the metrics within font files (doing so is a substantial 
task handled by \texttt{Wumpus.Basic} for the simple \texttt{AFM} 
font format). Instead, \wumpuscore makes do with operations based 
on measurements derived from the Courier fixed width font. 
Generally using metrics from a fixed width font over-estimates 
sizes for proportional fonts, in practice this is fine as 
\wumpuscore has limited needs.

\item[\texttt{Wumpus.Core.Geometry.}]
The usual types and operations from affine geometry - points, 
vectors and 3x3 matrices, also the \texttt{DUnit} type family.
Essentially this type family is a trick used heavily within 
\wumpuscore to avoid annotating class declarations with 
constraints on the unit of measurement (usually \texttt{Double}
representing the Point unit of publishing). With the 
\texttt{DUnit} trick, type constraints like 
\texttt{Fractional u} can be shifted to instance declarations 
rather than burden class declarations.

\item[\texttt{Wumpus.Core.GraphicProps.}]
Data types modelling the attributes of PostScript's graphics 
state (stroke style, dash pattern, etc.). Note that 
\wumpuscore labels all primitives - paths, text labels - with 
their drawing attributes, unlike PostScript there is no 
\emph{inheritance} of a Graphics State in \wumpuscore.

\item[\texttt{Wumpus.Core.OutputPostScript.}]
Functions to write PostScript and Encapsulated PostScript (EPS) 
files.

\item[\texttt{Wumpus.Core.OutputSVG.}]
Functions to write SVG files.

\item[\texttt{Wumpus.Core.Picture.}]
Operations to build \emph{pictures} - paths and labels within
an affine frame. Generally the functions here are convenience 
constructors for data types from the hidden module 
\texttt{Wumpus.Core.PictureInternal}. The data types from 
\texttt{PictureInternal} are exported with opaque signatures by 
\texttt{Wumpus.Core.WumpusTypes}.

\item[\texttt{Wumpus.Core.Text.Base.}]
Types for handling escaped \emph{special} characters within input 
text. Wumpus mostly follows SVG conventions for escaping strings, 
although glyph names should \emph{always} correspond to PostScript 
names and never XML / SVG ones, e.g. for \texttt{\&} use 
\texttt{\#ampersand;} not \texttt{\#amp;}. 

Also note, unless only SVG output is being generated, glyph 
names should be used rather than character codes. With PostScript, 
the resolution of character codes is dependent on the encoding of 
the font used to render it. As the core PostScript fonts use their 
own encoding rather than the common Latin1 encoding, using using 
numeric character codes (expected to be Latin1) can produce 
unanticipated results.

Unfortunately, even core fonts are often missing glyphs that 
familiarity with Unicode and Web publishing might expect them 
support. Generally, a PostScript renderer can do nothing about 
missing glyphs - it might print a space or an open, tall rectangle. 
As \wumpuscore is oblivious to the contents of fonts, it cannot 
issue a warning if a glyph is not present when it generates a 
document, so PostScript output must be proof-read if extended 
glyphs are used.

\item[\texttt{Wumpus.Core.Text.GlyphIndices.}]
An map of PostScript glyph names to Unicode code points.  

\item[\texttt{Wumpus.Core.Text.GlyphNames.}]
An map of Unicode code points to PostScript glyph names. 
Unfortunately this table is \emph{lossy} - some code points have 
more than one name, and as this file is auto-generated the 
resolution of which overlapping glyph name matches a code point is 
arbitrary. \wumpuscore uses this table only as a fallback if 
PostScript glyph name resolution cannot be solved through a font's 
encoding vector.

\item[\texttt{Wumpus.Core.Text.Latin1Encoding.}]
An encoding vector for the Latin 1 character set. 

\item[\texttt{Wumpus.Core.Text.StandardEncoding.}]
An encoding vector for the PostScript StandardEncoding set. This 
encoder is associated with the core text fonts - Helvetica, 
Courier and Times-Roman. Typically core fonts will include further 
glyphs not indexed by the Standard Encoding, for PostScript these 
glyphs are addressable only by name and not by index.

\item[\texttt{Wumpus.Core.Text.Symbol.}]
An encoding vector for the Symbol font which uses distinct glyph 
names. Unfortunately whilst the Symbol font is useful for 
PostScript, its use in SVG is actively discouraged by the W3C and
some browsers.

\item[\texttt{Wumpus.Core.VersionNumber.}]
Current version number of \wumpuscore.

\item[\texttt{Wumpus.Core.WumpusTypes.}]
This module collects internal types for Pictures, Paths etc. and
presents them as opaque types - i.e. their constructors are 
hidden. 
\end{description}

%-----------------------------------------------------------------
\section{Drawing model}
%-----------------------------------------------------------------

\wumpuscore has two main drawing primitives \emph{paths}
and text \emph{labels}. Ellipses are also a primitive although 
this is a concession to efficiency for drawing dots, which would 
otherwise require four Bezier arcs to describe. Paths are made 
from straight sections or Bezier curves, they can be open and 
\emph{stroked} to produce a line; or closed and \emph{stroked}, 
\emph{filled} or \emph{clipped}. Labels represent a single 
horizontal line of text - multiple lines must be composed from 
multiple labels and white-space other than the \texttt{space}
character should not be used.

Primitives are attributed with drawing styles - font name and 
point size for labels; line width, colour, etc. for paths. 
Primitives can be grouped to support support hyperlinks in SVG 
output (thus Primitives are not strictly \emph{primitive} as they 
are implemented with some nesting). The function \texttt{frame} 
assembles a list of primitives into a \texttt{Picture} with the 
standard affine frame where the origin is at (0,0) and the X 
and Y axes have the unit bases (i.e. they have a 
\emph{scaling value} of 1). 

\begin{figure}
\centering
\includegraphics{WorldFrame.eps}
\caption{The world frame, with origin at the bottom left.}
\end{figure}

\wumpuscore uses the same picture frame as PostScript where 
the origin at is the bottom left, see Figure 1. This contrasts to 
SVG where the origin is at the top-left. When \wumpuscore 
generates SVG, the whole picture is generated within a matrix 
transformation [ 1.0, 0.0, 0.0, -1.0, 0.0, 0.0 ] that changes the 
picture to use PostScript coordinates. This has the side-effect 
that text is otherwise drawn upside down, so \wumpuscore adds a 
rectifying transformation to each text element.

Once labels and paths are assembled as a \emph{Picture} they are
transformable with the usual affine transformations (scaling, 
rotation, translation).

Graphics properties (e.g colour) are opaque once primitives are
assembled into pictures - it is not possible to write a 
transformation function that turns elements in a picture blue. 
In some ways this is a limitation - for instance, the 
\texttt{Diagrams} library appears to support some notion of 
attribute overriding; however avoiding mutable attributes does 
keep this part of \wumpuscore conceptually simple. To make a 
blue or red triangle with \wumpuscore, one would make the drawing 
colour a parameter of the triangle constructor function.

%-----------------------------------------------------------------
\section{Affine transformations}
%-----------------------------------------------------------------

For affine transformations Wumpus uses the \texttt{Matrix3'3} data 
type to represent 3x3 matrices in row-major form. The constructor
\texttt{(M3'3 a b c  d e f  g h i)} builds this matrix:

\begin{displaymath}
\begin{array}{ccc}
a & b & c\\
d & e & f\\
g & h & i
\end{array}
\end{displaymath}

Note, in practice the elements \emph{g} and \emph{h} are 
largely superflous, but they are included in the data type so 
the matrix operations available (e.g. \texttt{invert} and 
\texttt{transpose}) have simple, regular definitions.

For example a translation matrix moving 10 units in the X-axis and
20 in the Y-axis will be encoded as 
 \texttt{(M3'3 1.0 0.0 10.0   0.0 1.0 20.0  0.0  0.0 1.0)}

\begin{displaymath}
\begin{array}{ccc}
1.0 & 0.0 & 10.0\\
0.0 & 1.0 & 20.0\\
0.0 & 0.0 & 1.0
\end{array}
\end{displaymath}

Affine transformations on Pictures are communicated to PostScript
as \texttt{concat} commands. For Pictures, \wumpuscore performs no
transformations itself, delegating all the work to PostScript or
SVG. Internally \wumpuscore transforms the bounding boxes of  
Pictures - the bounding box of a pictured is cached so that 
pictures can be composed with \emph{picture composition} operators 
like the \texttt{picBeside} combinator.

PostScript uses column-major form and uses a six element matrix
rather than a nine element one. The translation matrix above 
would produce this concat command:

\begin{verbatim}
[1.0 0.0 0.0 1.0 10.0 20.0] concat
\end{verbatim}

Similarly, it would be communicated to SVG via a 
\texttt{group} element:

\begin{verbatim}
<g transform="matrix(1.0, 0.0, 0.0, 1.0, 10.0, 20.0)"> ... </g>
\end{verbatim}

\wumpuscore also supports the regular affine transformations 
on Primitives (the arbitrary matrix transformation 
\texttt{transform} is not supported). Transformations are 
implicitly interpreted in the standard affine frame - origin at 
(0,0) and unit scaling vectors for the bases. 

For paths, all the transformations are precomputed on the control 
points before the output is generated. For labels and ellipses the 
\emph{start point} of the primitive (baseline-left for label, 
center for ellipse) is transformed by \wumpuscore and matrix 
operations are transmitted in the generated PostScript and SVG to 
transform the actual drawing (\wumpuscore has no access to the 
paths that describe character glyphs so it cannot precompute 
transformations on them).

One consequence of transformations operating on the control points
of primitives is that scalings do not scale the tip of the 
\emph{drawing pen}. If a path is stroked, lifted to a Picture and
then scaled the whole graphics state is effectively scaled 
including the pen tip so the path is drawn with a thicker outline. 
However, if a path is stoked and then scaled as a Primitive, the 
drawing pen is not scaled so the path will be drawn with the 
regular line width.

%-----------------------------------------------------------------
\section{Font handling}
%-----------------------------------------------------------------

Font handling is quite primitive in \wumpuscore. The bounding box 
of text label is only estimated - based on the length of the 
label's string rather than the metrics of the individual letters 
encoded in the font. Accessing the glyph metrics in a font 
requires a font loader - \texttt{wumpus-basic} has a font loader 
for the simple AFM font format but this is considered a special 
requirement and adds a lot of code\footnote{Also, although it is 
still useful, the AFM format is out-of-date, supporting the more 
universal TrueType / OpenType formats would be a great deal of 
work.}. As \wumpuscore is considered to be a fairly minimal system 
for generating pictures it can live without font metrics.

In PostScript, mis-named fonts can cause somewhat inscrutable 
printing anomalies depending on the implementation. At worst, 
GhostScript may do no subsequent drawing after a failing to load a 
font. SVG renderers fallback to some common font if a font cannot 
be found. 

A PostScript interpreter should have built-in support for the 
\emph{Core 14} fonts. For the \emph{writing} fonts, SVG renderers 
appear to support literal analogues quite well rather than needing 
general descriptors (e.g. \texttt{sans-serif} or 
\texttt{monospace}). The symbolic fonts Symbol and ZapfDingbats 
should be avoided for SVG - the W3C does not condone their use in 
SVG or HTML. Likewise, certain browsers reject them out of course.

The following table lists the Core 14 PostScript fonts and their 
SVG analogues, \texttt{wumpus-basic} includes a module 
\texttt{Wumpus.Basic.SafeFonts} encoding the fonts in this list 
and matching them to their appropriate encoding vectors.

\begin{tabular}{ l l }
PostScript name   & SVG name      \\
\hline
Times-Roman       & Times New Roman \\
Times-Italic      & Times New Roman - style="italic" \\
Times-Bold        & Times New Roman - font-weight="bold" \\
Times-BoldItalic  & Times New Roman - style="italic", font-weight="bold" \\
Helvetica         & Helvetica \\
Helvetica-Oblique & Helvetica - style="italic" \\
Helvetica-Bold    & Helvetica - font-weight="bold" \\
Helvetica-Bold-Oblique & Helvetica - style="italic", font-weight="bold" \\
Courier           & Courier New \\
Courier-Oblique   & Courier New - style="italic" \\
Courier-Bold      & Courier New - font-weight="bold" \\
Courier-Bold-Oblique & Courier New - style="italic", font-weight="bold" \\
Symbol            & (see text) \\
ZapfDingbats      & (see text) \\
\hline
\end{tabular}




%-----------------------------------------------------------------
\section{Acknowledgments}
%-----------------------------------------------------------------

PostScript is a registered trademark of Adobe Systems Inc.

\end{document}
