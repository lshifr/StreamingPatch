Package["Streaming`DataStructures`Interfaces`"]



PackageExport["Writer"]
PackageExport["WriterCreate"]
PackageExport["WriterInit"]
PackageExport["WriterNextWrite"]
PackageExport["WriterClose"]
PackageExport["WriterDataWrite"]



PackageImport["Streaming`Common`"] (* CleanUp *)


ClearAll[
	WriterCreate,
	Writer,
	WriterInit,
	WriterNextWrite,
	WriterClose,
	WriterDataWrite
];

WriterCreate[
	constructor_Function, 
	writeF_Function,
	activeQ_Function, 
	destructor_Function
]:=
	Writer[constructor,writeF,activeQ,destructor];


WriterInit[Writer[constructor_,_,_,_]]:= constructor[];

WriterNextWrite[Writer[_,writeF_,activeQ_,_], data_] /; activeQ[]:=
	With[{result = writeF[data]},
		If[result === $Failed, Message[WriterNextWrite::fail]];
		result
	];
WriterNextWrite[Writer[_,_,_,_],_]:=
	(
		Message[Writer::invld];
		$Failed	
	)

WriterClose[Writer[_,_,_,destructor_]]:=destructor[];

(*
**  Takes the writer object and a data scanner, which is a Scan-like function,
**  and applies a given function to data scanned in chunks. 
**  Writes the data obtained via the data scanner, using the writer object
*)
ClearAll[WriterDataWrite];
WriterDataWrite[writer_Writer, dataScanner_, opts : OptionsPattern[]]:=
	Module[{tag, catch, write},
		If[WriterInit[writer] === $Failed, Return[$Failed]];
		catch = Function[code, Catch[code, tag], HoldAll];
		write = 
			Function[
				data,
				Which[
					data === {}, (* Chunks may be empty - in which case, don't write *)
						Null,
					WriterNextWrite[writer, data] === $Failed,
						Throw[$Failed, tag],
					True,
						Null
				]				 
			];
		catch @ CleanUp[
			dataScanner[write],
			WriterClose[writer]
		]
	];
