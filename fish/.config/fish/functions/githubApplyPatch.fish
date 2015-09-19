function githubApplyPatch --argument link
	git apply (curl "$link".patch | psub)
end
