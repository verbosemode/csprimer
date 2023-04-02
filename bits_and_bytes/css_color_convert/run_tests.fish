#!/usr/bin/env fish

diff (dune exec --profile default ./bin/main.exe < color-convert/simple.css | psub) (cat color-convert/simple_expected.css | psub)
diff (dune exec --profile default ./bin/main.exe < color-convert/advanced.css | psub) (cat color-convert/advanced_expected.css | psub)

exit 0
