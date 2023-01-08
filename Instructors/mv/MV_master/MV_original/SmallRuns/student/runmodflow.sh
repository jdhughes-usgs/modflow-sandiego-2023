rm out
ln -s bcf.sml fort.11
ln -s rch.sml fort.12
ln -s pcg fort.13
ln -s oc fort.14
ln -s riv.sml fort.15
ln -s wel.case1.sml fort.16
ln -s vcont.2 fort.32
ln -s ibound.1.sml fort.95
modflow <bas.sml >out
cp fort.50 head-sml.bin
rm fort.11
rm fort.12
rm fort.13
rm fort.14
rm fort.15
rm fort.16
rm fort.32
rm fort.95
rm fort.50
