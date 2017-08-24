Package["Streaming`DataStructures`Interfaces`"]


PackageExport["AccumulatorDeclare"]
PackageExport["AccumulatorCreate"]
PackageExport["AccumulatorInit"]
PackageExport["AccumulatorAddElement"]
PackageExport["AccumulatorAddBatch"]
PackageExport["AccumulatorGetStorage"]
PackageExport["AccumulatorActiveQ"]
PackageExport["AccumulatorDelete"]
PackageExport["AccumulatorQ"]



ClearAll[
	AccumulatorDeclare,
	AccumulatorCreate,
	AccumulatorInit,
	AccumulatorAddElement,
	AccumulatorAddBatch,
	AccumulatorGetStorage,
	AccumulatorActiveQ,
	AccumulatorDelete,
	AccumulatorQ,
	$AccumulatorFormatting
];

AccumulatorQ[_]:=False;

$AccumulatorFormatting[_] = True;

AccumulatorDeclare[accumSymbol_Symbol, storageType_Symbol?StorageQ]:=
	Module[{},
		ClearAll[accumSymbol];

		accumSymbol /: 
			AccumulatorCreate[
				accumSymbol,
				chunkSizeLimit_,
				storage_storageType,
				elemSizeFunction_,
				splitter_
			]:=
		Module[{active = False, close, accumulatedSize = 0, accum = Internal`Bag[{}],sizeLim, spill},
			sizeLim = chunkSizeLimit /. Byte[lim_]:>lim;
			spill[addLast:True|False:False]:= 
				Module[{split = splitter[Internal`BagPart[accum, All], sizeLim], toAdd, rest},
					If[addLast, 
						toAdd = split;
						rest = {},
						(* else *)
						toAdd = Most @ split;
						rest = Last @ split;
					];
					Do[StorageAddBatch[storage, batch], {batch, toAdd}];
					accumulatedSize = If[rest === {}, 0, elemSizeFunction[rest]];
					accum = Internal`Bag[{}];
					Do[Internal`StuffBag[accum, elem],{elem, rest}];
				];
			close[]:=
				Module[{},
					StorageClose[storage];
					Remove[active, close, accumulatedSize, accum, sizeLim, spill];	
				];
			accumSymbol[
				Function[
					With[{result = StorageInit[storage]},
						If[!result, close[]];
						active = result
					]
				]
				,
				Function[
					elem
					,	
					Internal`StuffBag[accum, elem];
					accumulatedSize = elemSizeFunction[Internal`BagPart[accum, All]];
					If[accumulatedSize > sizeLim, 
						spill[]
					];
				]
				,
				Function[elems,
					Do[Internal`StuffBag[accum, elem], {elem, elems}];
					accumulatedSize=elemSizeFunction[Internal`BagPart[accum, All]];
					If[accumulatedSize > sizeLim, spill[]];	
				]
				,
				Function[
					spill[True];
					StorageExtract[storage]
				]
				,
				Function[TrueQ[active]]
				,
				Function[close[]]
			]
		];

		accumSymbol /: AccumulatorInit[accumSymbol[init_,_,_,_,activeQ_,_]] /; !activeQ[]:=
			If[TrueQ[init[]],
				True,
				(* else *)
				Message[AccumulatorInit::fail,accumSymbol];
				$Failed 
			];

		accumSymbol /:  AccumulatorInit[accum_accumSymbol] :=
			(
				Message[AccumulatorInit::active, accum];
				$Failed
			);

		accumSymbol /: 
			AccumulatorAddElement[acc : accumSymbol[_,add_,_,_, activeQ_, _], elem_] /; activeQ[]:=
				(add[elem]; acc);

		accumSymbol /: 
			AccumulatorAddElement[acc : accumSymbol[_,_,_,_,_,_], _]:= 
				(
					Message[AccumulatorAddElement::inactive, acc];
					$Failed
				);

		accumSymbol /:
			AccumulatorAddBatch[acc : accumSymbol[_,_,addBatch_,_, activeQ_, _], elems_List] /; activeQ[]:=
				(addBatch[elems]; acc);
	
		accumSymbol /:
			AccumulatorAddBatch[accumSymbol[_,_,_,_, activeQ_, _], elems_] /; activeQ[]:=
				(
					Message[AccumulatorAddBatch::nolst, elems];
					$Failed;
				);

		accumSymbol /: 
			AccumulatorAddBatch[acc:accumSymbol[_,_,_,_,_,_], _]:= 
				(
					Message[AccumulatorAddBatch::inactive, acc];
					$Failed
				);


		accumSymbol /: AccumulatorGetStorage[accumSymbol[_,_,_,get_, activeQ_, _]] /; activeQ[]:= get[];

		accumSymbol /: AccumulatorGetStorage[acc:accumSymbol[_,_,_,_,_,_]]:=
				(
					Message[AccumulatorGetStorage::inactive, acc];
					$Failed
				);

	    accumSymbol /: AccumulatorActiveQ[accumSymbol[_,_,_,_,activeQ_, _]]:= activeQ[];

		accumSymbol /: AccumulatorDelete[accumSymbol[_,_,_,_,activeQ_, destroy_]] /; activeQ[]:= destroy[];

		accumSymbol /: AccumulatorDelete[acc: accumSymbol[_,_,_,_,_,_]]:=
				(
					Message[AccumulatorDelete::inactive, acc];
					$Failed
				);

		
		accumSymbol /: AccumulatorQ[accumSymbol[_Function,_Function,_Function,_Function,_Function,_Function]] :=True;
		accumSymbol /: AccumulatorQ[accumSymbol] := True;
			
		
		accumSymbol /: MakeBoxes[storage: accumSymbol[_,_,_,_,activeQ_,_], fmt_] /; $AccumulatorFormatting[accumSymbol] :=
			With[{
				openbr = FromCharacterCode[171], 
    			closebr = FromCharacterCode[187], 
    			boxes = MakeBoxes[#]& @ accumSymbol[Dynamic[If[activeQ[], "Active", "Inactive"]]]
				},
				InterpretationBox[
    				RowBox[{openbr, boxes, closebr}],
      				storage
				]	
			]
		];

AccumulatorDeclare[_Symbol, st_Symbol]:=
	(
		Message[AccumulatorDeclare::badstrg, st];
		$Failed;
	);
	