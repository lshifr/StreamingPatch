Package["Streaming`DataStructures`Interfaces`"]



(******************************************************************************)
(************ 		Strongly typed parametrized Set		 		***************)
(******************************************************************************)

PackageExport["SetDeclare"]
PackageExport["SetAdd"]
PackageExport["SetRemove"]
PackageExport["SetToList"]
PackageExport["SetSize"]
PackageExport["SetMemberQ"]



ClearAll[SetDeclare, SetAdd, SetRemove, SetToList];

SetAdd::badtype =  "The type of (some of the) elements `1` is incompatible with the set type `2`, \
which stores elements of the type `3`";
SetAdd::notype = "`1` is not a valid set type ";
SetRemove::badtype = "The type of (some of the) elements `1` is incompatible with the set type `2`, \
which stores elements of the type `3`";
SetRemove::notype = "`1` is not a valid set type ";
SetToList::notype = "`1` is not a valid set type ";
SetSize::notype = "`1` is not a valid set type ";
SetMemberQ::notype = "`1` is not a valid set type ";

SetAdd[sym_Symbol[__], _, _:None]:=
	(
		Message[SetAdd::notype, sym];
		ThrowError[SetAdd]
	);
	
SetRemove[sym_Symbol[__], _, _:None]:=
	(
		Message[SetRemove::notype, sym];
		ThrowError[SetRemove]
	);
	
SetToList[sym_Symbol[__]]:=	
	(
		Message[SetToList::notype, sym];
		ThrowError[SetToList]
	);
	
SetSize[sym_Symbol[__]]:=	
	(
		Message[SetSize::notype, sym];
		ThrowError[SetSize]
	);
	
SetMemberQ[sym_Symbol[__],_]:=	
	(
		Message[SetMemberQ::notype, sym];
		ThrowError[SetMemberQ]
	);				

SetDeclare[setSymbol_Symbol, elementHead_Symbol, onNewlyAddedElementFunction_:Identity]:=
	Module[{},
		
		ClearAll[setSymbol];
		setSymbol::badtype = "The type of (some of the) elements `1` is incompatible with the set type `2`, \
which stores elements of the type `3`";
		
		setSymbol[{elems___elementHead}]:=
			setSymbol[Association[Thread[{elems} -> True]], elementHead];
			
		setSymbol[{elems___}]:=	
			(
				Message[setSymbol::badtype, {elems}, setSymbol,  elementHead];
				ThrowError[setSymbol];
			);
			
		setSymbol /: SetAdd[
			set:setSymbol[_,elementHead], 
			elem_elementHead, 
			onNewlyAddedF_: onNewlyAddedElementFunction
		]:=
			SetAdd[set,{elem}, onNewlyAddedF];
			
		setSymbol /: SetAdd[
			setSymbol[assoc_Association,elementHead], 
			elems:{___elementHead}, 
			onNewlyAddedF_: onNewlyAddedElementFunction
		]:=
			With[{newElems = Select[elems, !KeyExistsQ[assoc, #]&]},
				With[{result = setSymbol[Append[assoc,Thread[newElems -> True]], elementHead]},
					Scan[onNewlyAddedF, newElems];
					result
				]
			];
			
		setSymbol /: SetAdd[setSymbol[_,elementHead], elems_List, _:onNewlyAddedElementFunction]:=
			(
				Message[SetAdd::badtype, elems, setSymbol, elementHead];
				ThrowError[SetAdd]
			);
		
			
		setSymbol /: SetRemove[setSymbol[assoc_Association,elementHead], elems:{__elementHead}]:=
			setSymbol[KeyDrop[assoc, elems], elementHead];
			
		setSymbol /: SetRemove[setSymbol[assoc_Association,elementHead], elems_List]:=
			(
				Message[SetRemove::badtype, elems, setSymbol, elementHead];
				ThrowError[SetAdd]
			);
			
		setSymbol /: SetToList[setSymbol[assoc_Association,elementHead]]:= Keys[assoc];
		
		setSymbol /: SetSize[setSymbol[assoc_Association,elementHead]]:= Length[assoc];
		
		setSymbol /: SetMemberQ[setSymbol[assoc_Association,elementHead], elem_]:=KeyExistsQ[assoc, elem];
		
	];
