(* ::Package:: *)

c1=-5674.5359;
c2=6.3925247;
c3=-0.9677843*10^-2;
c4=0.62215701*10^-6;
c5=0.20747825*10^-8;
c6=-0.9484024*10^-12;
c7=4.1635019;
c8=-5800.2206;
c9=1.3914993;
c10=-0.048640239;
c11=0.41764768*10^-4;
c12=-0.14452093*10^-7;
c13=6.5459673;

B=101.325*10^3;


T[t_]:=273.15+t
htd[t_,d_]:=1.005 t+(2500+1.84 t) d/1000
dp[pq_]:=622 pq/(B-pq)
Pqb[t_]:=Piecewise[{{Exp[c8/T[t]+c9+c10 T[t]+c11 T[t]^2+c12 T[t]^3] T[t]^c13,0<=t<200},
	{Exp[c1/T[t]+c2+c3 T[t]+c4 T[t]^2+c5 T[t]^3+c6 T[t]^4] T[t]^c7,-100<t<0}}]


psy[a_->x_,b_->y_]:=Block[{setvalue=Piecewise[{{x,If[Head@a===String,a,ToString@HoldForm@a]==#},
	{y,If[Head@b===String,b,ToString@HoldForm@b]==#}},Null]&,
		t,d,\[CurlyPhi],h,ts,tl,
		tf,df,tsf,tlf},
	{t,d,\[CurlyPhi],h,ts,tl}=setvalue/@{"t","d","\[CurlyPhi]","h","ts","tl"};
	If[Count[{t,d,\[CurlyPhi],h,ts,tl},Null]!=4,Return["error"]];
	If[ts=!=Null,h=htd[ts,dp@Pqb@ts]];
	If[tl=!=Null,d=dp@Pqb@tl];
	If[Count[{t,d,h},Null]==1,
		Which[t===Null,t=InverseFunction[htd[#,d]&]@h,
			d===Null,d=InverseFunction[htd[t,#]&]@h,
			h===Null,h=htd[t,d]],
		Which[t=!=Null,d=dp[\[CurlyPhi]/100 Pqb@t];h=htd[t,d],
			d=!=Null,t=tf/.FindRoot[dp[\[CurlyPhi]/100 Pqb@tf]==d,{tf,0}];h=htd[t,d],
			h=!=Null,{t,d}={tf,df}/.FindRoot[{htd[tf,df]==h,dp[\[CurlyPhi]/100Pqb@tf]==df},{{tf,0},{df,0}}]]];
	If[\[CurlyPhi]===Null,\[CurlyPhi]=100InverseFunction[dp]@d/Pqb@t];
	If[ts===Null,ts=tsf/.FindRoot[htd[tsf,dp@Pqb@tsf]==h,{tsf,t}]];
	If[tl===Null,tl=tlf/.FindRoot[dp@Pqb@tlf==d,{tlf,t}]];
	Association@@Thread@Rule[{"t","d","\[CurlyPhi]","h","ts","tl"},{t,d,\[CurlyPhi],h,ts,tl}]
]
SetAttributes[psy,HoldAll]


(*  ::Example::  *)


(*\:7ed8\:5236\:7113\:6e7f\:56fe*)

plotrange={{0,40},{-30,70}};
regfun=Function[{d,h},0<d<dp[Pqb[InverseFunction[htd[#,d]&][h+d Tan[68\[Degree]]]]]];
trans[{d_,h_}]:={d,h-d Tan[68\[Degree]]}

psychrometric=Show[
	ParametricPlot[Table[trans@{d,htd[t,d]},{t,-30,70,5}],{d,-50,50},RegionFunction->regfun,PlotStyle->RGBColor[0.37,0.51,0.71]],(*\:7b49\:6e29\:7ebf*)
	ParametricPlot[Table[trans@{d,h},{h,-30,220,10}],{d,-30,50},RegionFunction->regfun,PlotStyle->RGBColor[0.88,0.61,0.14]],(*\:7b49\:7113\:7ebf*)
	ParametricPlot[Table[{d,y},{d,0,70,5}],{y,-30,80},RegionFunction->regfun,PlotStyle->RGBColor[0.56,0.69,0.19]],(*\:7b49\:542b\:6e7f\:91cf\:7ebf*)
	ParametricPlot[Table[trans@{#,htd[t,#]}&@dp[Pqb[t]*\[CurlyPhi]],{\[CurlyPhi],0.05,1,0.05}],{t,-30,75},PlotStyle->RGBColor[0.53,0.47,0.70]],(*\:7b49\:76f8\:5bf9\:6e7f\:5ea6\:7ebf*)
	PlotRange->plotrange,AspectRatio->1];


(*\:6839\:636e\:4efb\:610f\:4e24\:53c2\:6570\:6c42\:72b6\:6001\:70b9*)
(*t    \:5e72\:7403\:6e29\:5ea6(\[Degree]C)
  d    \:542b\:6e7f\:91cf(g/kg)
  \[CurlyPhi]    \:76f8\:5bf9\:6e7f\:5ea6(%)
  h    \:7113(kJ/kg)
  ts   \:6e7f\:7403\:6e29\:5ea6(\[Degree]C)
  tl   \:9732\:70b9\:6e29\:5ea6(\[Degree]C)*)

pt1=psy[t->25,\[CurlyPhi]->100]
pt2=psy["h"->77,"\[CurlyPhi]"->80]


(*\:6c42\:4e24\:72b6\:6001\:70b9\:6df7\:5408*)
ratio=75;(*\:6df7\:5408\:6bd4\:4f8b*)

psy["h"->(pt1["h"]*ratio+pt2["h"]*(1-ratio))/100,"d"->(pt1["d"]*ratio+pt2["d"]*(1-ratio))/100]


(*\:6c42\:9001\:98ce\:72b6\:6001\:70b9*)
\[CurlyEpsilon]=10000;(*\:70ed\:6e7f\:6bd4(J/g)*)
\[CurlyPhi]l=95;(*\:673a\:5668\:9732\:70b9*)

psy@@FindRoot[{dp[\[CurlyPhi]l/100Pqb[t]]==df,d==df}/.
NSolve[{(hf-pt2["h"])/(df-pt2["d"])==\[CurlyEpsilon]/1000,htd[t,df]==hf},{hf,df}][[1]],
	{{t,pt2["t"]},{d,pt2["d"]}}]
