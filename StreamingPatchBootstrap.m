ClearAll[DownloadAndInstallStreamingPatch];
DownloadAndInstallStreamingPatch::dnldfail = "Patch download failed";
DownloadAndInstallStreamingPatch::corrpt = "The downloaded patch seems to be corrupt";
DownloadAndInstallStreamingPatch[]:=
    Module[{temp, dir, mainDir},
		    temp = Quiet @ URLDownload["https://github.com/lshifr/StreamingPatch/archive/master.zip"];
		    If[temp === $Failed,
			     Message[DownloadAndInstallStreamingPatch::dnldfail];
			     Return[$Failed]
		    ];
		    dir = Quiet @ First @ ExtractArchive[temp, $TemporaryDirectory];
		    If[dir === $Failed,
			     Message[DownloadAndInstallStreamingPatch::corrpt];
			     Return[$Failed];
		    ];
		    mainDir = FileNameJoin[{$UserBaseDirectory, "Applications", "StreamingPatch"}];
		    If[DirectoryQ[mainDir], DeleteDirectory[mainDir, DeleteContents->True]];
		    CopyDirectory[dir, mainDir];
		    DeleteDirectory[dir, DeleteContents->True];
		    DeleteFile[temp];
	];
