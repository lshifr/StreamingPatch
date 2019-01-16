
Needs["Streaming`"]

Block[{$Path = Prepend[$Path, DirectoryName @ DirectoryName @ $InputFileName]},
    Quiet @ Get["Interfaces`"]
]

Scan[
    ToExpression[
		    #,
		    StandardForm,
		    Function[
            sym,
            sym = Symbol @ StringJoin[
                "Streaming`DataStructures`Interfaces`",
                SymbolName[Unevaluated @ sym]
            ],
			      HoldAll
		    ]
	 ]&,
   Flatten @ StringCases[
       Flatten @ Map[
           Names["Streaming`*`*`"<> # ]&,
           Names["Streaming`DataStructures`Interfaces`*"]
       ],
       __ ~~ "PackagePrivate`"~~__
   ]
];

Streaming`Components`DataChunk`DataChunk /: Streaming`Objects`Constructor[
	Streaming`Components`DataChunk`DataChunk
] =  Function[
	Null,
	With[{self=#1,args=##2},
		Streaming`Objects`Objects`PackagePrivate`bindFields[
			self,
			Streaming`Components`DataChunk`DataChunk,
			{
				{Set,Hold[OO`directory],Hold[Streaming`LazyList`$StreamingCacheBase]},
				{Set,Hold[OO`type],Hold["File"]},
				{Set,Hold[OO`refCounter],Hold[0]},
				{Set,Hold[OO`metaData],Hold[<||>]},
				{Set,Hold[OO`cacheBareChunk],Hold[True]},
				{Set,Hold[OO`deleteFileUponDestruction],Hold[True]}
			},
			True
		];
		If[Streaming`Objects`HasMethod[self,Streaming`Objects`NewObject],
			Streaming`Objects`Objects`PackagePrivate`MethodsImpl[
				Streaming`Components`DataChunk`DataChunk,Streaming`Objects`NewObject
			][self,args];
			self,
			Streaming`Objects`Constructor[
				Streaming`Objects`SuperType[Streaming`Components`DataChunk`DataChunk]
			][self]
		]
	]
];


StorageDeclare[
    Streaming`DataStructures`DataTypes`MemoryConstrainedStorage,
    MatchQ[#,_List]&
];

StorageDeclare[
    Streaming`LazyList`LazyListStorage,
    MatchQ[#,_?Streaming`LazyList`LazyListValidQ]&
];

AccumulatorDeclare[
    Streaming`LazyList`LazyListBackedAccumulator,
    Streaming`LazyList`LazyListStorage
];

IteratorDeclare[
	Streaming`Components`Chunking`DataChunkIterator,
	None,
	Function[ch,Streaming`Components`DataChunk`DataChunkQ[ch]||ch===None]
];

IteratorDeclare[
	Streaming`Components`Chunking`StreamingBatchIterator,
	None,
	Streaming`Components`Chunking`StreamingBatchQ[#]||#===None&
];

IteratorDeclare[Streaming`DataStructures`DataTypes`ListIterator,{},ListQ];

SetDeclare[
    Streaming`LazyList`AddedChunksSet,
    Streaming`LazyList`Chunking`LazyListDataChunk,
    Identity
];

SetDeclare[
    Streaming`LazyList`LazyList`PackagePrivate`InnerLazyListsSet,
    Streaming`LazyList`LazyList,
    Identity
];

Streaming`Components`DataChunk`$DataChunkDirectory =
    Streaming`LazyList`$StreamingCacheBase;

If[!DirectoryQ[Streaming`LazyList`$StreamingCacheBase],
    CreateDirectory[Streaming`LazyList`$StreamingCacheBase]
]


Streaming`DataStructures`DataTypes`ListIterator /: MakeBoxes[
	liter_Streaming`DataStructures`DataTypes`ListIterator, 
	StandardForm
] :=
	BoxForm`ArrangeSummaryBox[
		Streaming`DataStructures`DataTypes`ListIterator,
		liter,
		$IteratorIcon,
		{
			BoxForm`MakeSummaryItem[{"Type ", List}, StandardForm],
			BoxForm`MakeSummaryItem[{
				"Status: ", 
				Dynamic[If[TrueQ @ IteratorActiveQ[liter], "Active", "Inactive"]]
			}, StandardForm]
		}
    	,
		{}, 
    	StandardForm
    ]	

$ContextPath = DeleteCases[$ContextPath, "Streaming`DataStructures`Interfaces`"];
