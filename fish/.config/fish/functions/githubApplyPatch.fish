function githubApplyPatch --argument link
	git apply (curl -L "$link.patch" | psub)
end
