# Video revision 13

The tutorial now shows normal CRAN installation and version-neutral narration,
labels, and captions. The companion scripts use the same executable code and
source example as revision 6; changes to demo.R are comments only. The README
specifies sas2r 0.6.0 or later and explains what to do if CRAN supplies an older
version. The release itself is managed separately.

The existing live-run evidence is preserved. PR #65 was subsequently squash
merged into develop as 690b68fbc4a376d423cc7dc948f03116f753775e. The historical
OpenAI run was not performed against a CRAN release. No additional paid
provider run was made for this narration/instructions revision.

Validation: TypeScript and ESLint; 86 caption texts, timing and narration
consistency; exact companion/screen-source agreement; commented YAML template
copy check; R syntax and offline setup/input checks. The offline check used
the locally installed merged develop build. It made no provider requests.
