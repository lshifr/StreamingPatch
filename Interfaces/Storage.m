Package["Streaming`DataStructures`Interfaces`"]



PackageExport["StorageDeclare"]
PackageExport["StorageCreate"]
PackageExport["StorageInit"]
PackageExport["StorageAddBatch"]
PackageExport["StorageExtract"]
PackageExport["StorageClose"]
PackageExport["StorageActiveQ"]
PackageExport["StorageQ"]


ClearAll[
	StorageDeclare,
	StorageCreate,
	StorageInit,
	StorageAddBatch,
	StorageExtract,
	StorageClose,
	StorageActiveQ,
	StorageQ
];

StorageQ[_]:=False;

ClearAll[$StorageFormatting];
$StorageFormatting[_] = True;


ClearAll[StorageDeclare];
StorageDeclare[stsym_Symbol, stResultTypeTest_:(True&)]:=
	Module[{},
		ClearAll[stsym];
			
		stsym /: 
			StorageCreate[
				stsym, 
				constructor_Function, 
				append_Function,
				extract_Function,
				activeQ_Function, 
				destructor_Function
			]:= stsym[constructor, append, extract, activeQ, destructor];


		stsym /:  StorageInit[stsym[constructor_, _, _, activeQ_, _]] /; !activeQ[]:=
			If[
				TrueQ[constructor[]], 
				True,
				(* else *)	
				Message[StorageInit::fail,stsym];
				$Failed 
			];

		stsym /:  StorageInit[accum_stsym] :=
			(
				Message[StorageInit::active, accum];
				$Failed
			);

		stsym /: StorageAddBatch[stsym[_, append_, _, activeQ_, _], elems_List] /; activeQ[] :=
			append[elems];
	
		stsym /: StorageAddBatch[stsym[_, append_, _, activeQ_, _], elems_] /; activeQ[] :=
			(
				Message[StorageAddBatch::nolst, elems];
				$Failed
			);

		stsym /: StorageAddBatch[accum_stsym, _]:=
			(
				Message[StorageAddBatch::inactive, accum];
				$Failed
			);

		stsym /: StorageExtract[stsym[_, _, extract_, activeQ_, _]] /; activeQ[] :=
			With[{result = extract[]},
				If[stResultTypeTest[result], 
					result,
					(* else *)
					Message[StorageExtract::err, stsym, result];
					$Failed
				]
			];

		stsym /: StorageExtract[accum_stsym]:=
			(
				Message[StorageExtract::inactive, accum];
				$Failed
			);

		
		stsym /: StorageClose[stsym[_, _, _, activeQ_, destructor_]] /; activeQ[] :=
			destructor[];

		
		stsym /: StorageClose[accum_stsym]:=
			(
				Message[StorageClose::inactive, accum];
				$Failed
			);

		stsym /: StorageActiveQ[stsym[_,_,_, activeQ_,_]]:= activeQ[];

		stsym /: StorageQ[stsym[_Function,_Function,_Function,_Function,_Function]]:= True;

		stsym /: StorageQ[stsym] = True;

		stsym /: MakeBoxes[storage: stsym[_,_,_,activeQ_,_], fmt_] /; $StorageFormatting[stsym] :=
			With[{
				openbr = FromCharacterCode[171], 
    			closebr = FromCharacterCode[187], 
    			boxes = MakeBoxes[#]& @ stsym[Dynamic[If[activeQ[], "Active", "Inactive"]]]
				},
				InterpretationBox[
    				RowBox[{openbr, boxes, closebr}],
      				storage
				]	
			]
	]
	