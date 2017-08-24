Package["Streaming`DataStructures`Interfaces`"]

(******************************************************************************)
(************ 		Generic parametrized iterator interface		***************)
(******************************************************************************)


PackageExport["IteratorDeclare"]
PackageExport["IteratorCreate"]
PackageExport["IteratorInit"]
PackageExport["IteratorNext"]
PackageExport["IteratorClose"]
PackageExport["IteratorEndValueQ"]
PackageExport["IteratorActiveQ"]
PackageExport["$IteratorFormatting"]
PackageExport["IteratorEndValue"]
PackageExport["$IteratorIcon"]



ClearAll[
	IteratorDeclare,
	IteratorCreate,
	IteratorInit,
	IteratorNext,
	IteratorClose,
	IteratorEndValueQ,
	IteratorActiveQ,
	IteratorEndValue
];


$IteratorFormatting[_]=True;

IteratorNext::itererr = "Iterator of type `1` element type error";
IteratorNext::inactive = "Iterator `1` is inactive";
IteratorInit::active = "Iterator `1` is already active";
IteratorInit::fail = "Iterator construction failed, for the iterator of type `1`";
IteratorClose::inactive = "Iterator `1` is inactive";

IteratorDeclare[iteratorSymbol_Symbol, endExpression_:None, iteratorElementTest_:(True &)]:=
	Module[{},
		ClearAll[iteratorSymbol];
		
		iteratorSymbol /: 
			IteratorCreate[
				iteratorSymbol,
				constructor_Function,
				next_Function, 
				activeQ_Function, 
				cleanup_Function
			]:= 
				iteratorSymbol[constructor,next,activeQ,cleanup];

		iteratorSymbol /: IteratorInit[iteratorSymbol[constructor_, _, activeQ_, _ ]] /; !activeQ[]:=
			If[
				TrueQ[constructor[]], 
				True,
				(* else *)	
				Message[IteratorInit::fail,iteratorSymbol];
				$Failed 
			];
		
		iteratorSymbol /:  IteratorInit[iter_iteratorSymbol] :=
			(
				Message[IteratorInit::active, iter];
				$Failed
			);
		
		iteratorSymbol /: IteratorNext[iteratorSymbol[ _, next_, activeQ_, _], args___] /; activeQ[]:=
			With[{result = next[args]},
				result /; iteratorElementTest[result]
			];
		
		iteratorSymbol /: IteratorNext[iteratorSymbol[_, _, activeQ_, _]] /; activeQ[] :=
			(
				Message[IteratorNext::itererr, iteratorSymbol];
				$Failed
			);
			
		iteratorSymbol /: IteratorNext[iter: iteratorSymbol[_, _, _, _]]:=	
			(
				Message[IteratorNext::inactive, iter];
				$Failed
			);
			
		iteratorSymbol /: IteratorEndValueQ[_iteratorSymbol, val_]:=
			val === endExpression;
			
		iteratorSymbol /: IteratorEndValue[iteratorSymbol]:= endExpression;
			
		iteratorSymbol /: IteratorClose[iteratorSymbol[_,_,activeQ_,cleanup_]] /; activeQ[]:= 
			cleanup[];
			
		iteratorSymbol /: IteratorClose[iter_iteratorSymbol]:=
			(
				Message[IteratorClose::inactive, iter];
				$Failed
			);
			
		iteratorSymbol /: IteratorActiveQ[iteratorSymbol[_,_,activeQ_,_]]:= activeQ[];
		
		(*
		
		iteratorSymbol /: MakeBoxes[iter: iteratorSymbol[_,_,activeQ_,_], fmt_] /; $IteratorFormatting[iteratorSymbol] :=
			With[{
				openbr = FromCharacterCode[171], 
    			closebr = FromCharacterCode[187], 
    			boxes = MakeBoxes[#]& @ iteratorSymbol[Dynamic[If[activeQ[], "Active", "Inactive"]]]
				},
				InterpretationBox[
    				RowBox[{openbr, boxes, closebr}],
      				iter
				]	
			]
		*)
		
	];	
	

$IteratorIcon = 
	Graphics[{
		White, 
			Rectangle[{0, 0}, {7, 3}],
			{EdgeForm[Directive[Black]], Rectangle[{1, 1}, {2, 2}]},
			{EdgeForm[Directive[Black]], Rectangle[{3, 1}, {4, 2}]},
			{EdgeForm[Directive[Black]], Rectangle[{5, 1}, {6, 2}]},
			{Black, Arrowheads[Small], Arrow[{{1, 0.5}, {7, 0.5}}]}
		}, 
		Background -> GrayLevel[0.93],
		ImageSize -> {
			Automatic,
			Dynamic[3.5*(CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification])]
		},
		Frame -> {{True, True}, {True, True}},
		FrameStyle -> Directive[Thickness[Tiny], GrayLevel[0.7]],
		FrameTicks -> {{None, None}, {None, None}}, 
		GridLines -> {None, None}
  	];	
	