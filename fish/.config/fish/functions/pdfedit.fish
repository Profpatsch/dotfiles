function pdfedit
	set fname "$argv[1]"
	set TMP (mktemp -d)
	pdfseparate "$fname" $TMP/p%d.pdf

	for p in $TMP/p*.pdf
		inkscape "$p"
	end
	pdfjam --outfile (dirname $fname)/(basename $fname .pdf)-edited.pdf $TMP/p*.pdf
end
