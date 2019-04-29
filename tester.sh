for i in ./*.lat
do
    b=$(basename $i .lat)
    (./shlang ./$b.lat | diff - ./$b.output || echo $i)
done
