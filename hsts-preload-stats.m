(* ::Package:: *)

(* ::Section:: *)
(*Data Handling*)


Clear[stableData]
stableDataSource := 
  stableData = 
   Import["https://omahaproxy.appspot.com/history?channel=stable"];


stableData = Join[stableDataSource, {
	{"win", "stable", "49.0.2623.54", "2016-03-08 12:00:00"},
	{"win", "stable", "50.0.2654.0", "2016-04-19 12:00:00"}
}];


Clear[RetrieveFile]
defaultGitDir = FileNameJoin[{$HomeDirectory, "/chromium/src/.git"}];
RetrieveFile[revision_, filePath_, gitDir_: defaultGitDir] := 
 RetrieveFile[gitDir, revision, filePath] = With[
	{output = RunProcess[{"git", "--git-dir=" ~~ gitDir, "show", revision ~~ ":" ~~ filePath}]},
	If[output["ExitCode"] == 128, Null, output["StandardOutput"]]
 ]


Clear[RetrievePreloadList]
RetrievePreloadList[version_String] := 
 RetrievePreloadList[version] = Module[{try},
   try = RetrieveFile[version, "net/http/transport_security_state_static.json"];
   If[try =!= Null, Return[try]];
   
   (* Until 536fd0b69edf4b73f057fe9c79f15beddd239ef8 *)
   
   try = RetrieveFile[version, "net/base/transport_security_state_static.json"];
   If[try =!= Null, Return[try]];
   
   (* Until 78d5cd44086799941bcd1c441a7a78ccb7e647f1 *)
   
   try = RetrieveFile[version, "net/base/hsts_preloaded.json"];
   If[try =!= Null, Return[try]];
   
   (* Until 236ca9fbfa1cc341bb185a74a7c390912e51cb71 *)
   (* net/base/transport_security_state_static.h *)
   Return[Null];
   ]


(* Assumes comments appear at the beginning of a line. This is true for the preload list file. *)

SanitizeHSTSJSON[s_] := 
 StringReplace[
  StringReplace[s, 
   Longest[StartOfLine ~~ RepeatedNull[Whitespace] ~~ "//" ~~ 
      RepeatedNull[Except["\n"]] ~~ RepeatedNull["\n"]] -> ""], 
  "\n" -> ""]


Clear[Entries]
Entries[version_String] := Entries[version] = Module[{listStr},
    listStr = RetrievePreloadList[version];
    If[listStr === Null,
     {},
     "entries" /. 
      ImportString[RetrievePreloadList[version] // SanitizeHSTSJSON, 
       "JSON"]
     ]
    ];


(* ::Section:: *)
(*Processing*)


hstsPattern = {___, "mode" -> "force-https", ___};
pinningPattern = {___, "pins" -> _, ___};


(* ::Input:: *)
(* Dynamic[currentRow] *)


ProcessRow[{os_, channel_, version_, date_}] := (
  currentRow = version;
  {
   "date" -> DateString[date],
   "version" -> version,
   "numEntries" -> Length[Entries[version]],
   "numHSTS" -> Length[Cases[Entries[version], hstsPattern]],
   "numPinned" -> Length[Cases[Entries[version], pinningPattern]],
   "numHSTSAndPinned" -> Length[Cases[Cases[Entries[version], hstsPattern], pinningPattern]]
   }
  )


winStableData = 
  SortBy[Select[stableData, First[#] == "win" &], #[[4]] &];


firstReleaseForEachVersion = DeleteDuplicatesBy[
	winStableData, 
	First[StringSplit[#[[3]], "."]] &
];


winResults = Cases[
	ProcessRow /@ firstReleaseForEachVersion, 
	Except[{___, "numEntries" -> 0, ___}]
];


winResults//TableForm


(* ::Section:: *)
(*Visualization*)


(* ::Subsection:: *)
(*Dots*)


plotDots = DateListPlot[
  {
   Legended[{"date", "numHSTS"} /. winResults, "HSTS"],
   Legended[{"date", "numPinned"} /. winResults, "Pinned"],
   Legended[{"date", "numHSTSAndPinned"} /. winResults, "HSTS + Pinned"]
   },
  Joined -> False,
  PlotLabel -> "Number of Chromium HSTS preload entries \[Dash] Stable Releases\nLucas Garron - February 18, 2016\n\n(Including projections for Chrome 49 and Chrome 50.)\n",
  ImageMargins -> 20,
  PlotRange -> All
]


Export[FileNameJoin[{NotebookDirectory[], "hstsPreloadGrowth.svg"}], plotDots];


(* ::Subsection:: *)
(*Bar Chart*)


barChart = BarChart[
  Table[Labeled[{
      "numPinned" - "numHSTSAndPinned",
      "numHSTSAndPinned",
      "numHSTS" - "numHSTSAndPinned"
      } /. results,
    Rotate[
     StringSplit[("version" /. results), "."] // First, \[Pi]/2]
    ], {results, winResults}],
  ChartLegends -> {"Only Pinned", "HSTS + Pinned", "Only HSTS"},
  ChartLayout -> "Stacked",
  PlotLabel -> "Number of Chromium HSTS preload entries\nChart by Lucas Garron - February 17, 2016\n",
  ImageMargins -> 20,
  ImageSize -> 512
]


Export[FileNameJoin[{NotebookDirectory[], "hstsPreloadGrowthBarChart.svg"}], barChart];
