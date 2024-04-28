
sml @SMLload=compiler.amd64-linux example.tig
cat ../../src/runtime/* example.tig.s  > example.s
echo -e "\tjal\ttig_exit\n" >> example.s

