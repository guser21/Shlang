for i in ./*.shl
do
    b=$(basename $i .shl)
    (./../interpreter ./$b.shl | diff - ./$b.output || echo $i)
done
