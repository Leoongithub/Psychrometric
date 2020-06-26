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

plotrange={{0,40},{-30,70}};
regfun=Function[{d,h},0<d<dp[Pqb[InverseFunction[htd[#,d]&][h+d Tan[68\[Degree]]]]]];

B=101.325*10^3;


T[t_]:=273.15+t
htd[t_,d_]:=1.005 t+(2500+1.84 t) d/1000
dp[pq_]:=622 pq/(B-pq)
Pqb[t_]:=Piecewise[{{Exp[c8/T[t]+c9+c10 T[t]+c11 T[t]^2+c12 T[t]^3] T[t]^c13,0<=t<200},{Exp[c1/T[t]+c2+c3 T[t]+c4 T[t]^2+c5 T[t]^3+c6 T[t]^4] T[t]^c7,-100<t<0}}]
trans[{d_,h_}]:={d,h-d Tan[68\[Degree]]}


psychrometric=Show[
ParametricPlot[Table[trans@{d,htd[t,d]},{t,-30,70,5}],{d,-50,50},RegionFunction->regfun,PlotStyle->Blue],
ParametricPlot[Table[trans@{d,h},{h,-30,220,10}],{d,-30,50},RegionFunction->regfun,PlotStyle->Blue],
ParametricPlot[Table[{d,y},{d,0,70,5}],{y,-30,80},RegionFunction->regfun,PlotStyle->Blue],
ParametricPlot[Table[trans@{#,htd[t,#]}&@dp[Pqb[t]*\[CurlyPhi]],{\[CurlyPhi],0.05,1,0.05}],{t,-30,75},PlotStyle->Blue],
PlotRange->plotrange,AspectRatio->1];


psy[a_->x_,b_->y_]:=Block[{setvalue=Piecewise[{{x,If[Head@a===String,a,ToString@HoldForm@a]==#},{y,If[Head@b===String,b,ToString@HoldForm@b]==#}},Null]&,
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
{t,d,\[CurlyPhi],h,ts,tl}
]
SetAttributes[psy,HoldAll]
