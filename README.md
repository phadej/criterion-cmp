# criterion-cmp

Compare criterion results in text mode

Collect benchmarks using `--csv` flag to `criterion` benchmarks.
Beware, `criterion` appends the results, so CSV file may be "corrupted" that way.

```
% cabal run -- criterion-cmp old.csv long-name-new.csv

Benchmark                   old       long-name-new
alter                       1.027e-3  0.990e-3  -3.62%
delete                      2.070e-4  2.157e-4  +4.19%
foldlWithKey                0.945e-3  0.950e-3  +0.50%
foldlWithKey'               2.391e-5  2.375e-5  -0.69%
foldrWithKey                0.458e-7  0.460e-7  +0.37%
fromAscList                 1.820e-4  1.828e-4  +0.45%
fromDistinctAscList         1.026e-4  1.054e-4  +2.74%
fromList                    2.501e-4  2.541e-4  +1.61%
insert                      2.529e-4  2.557e-4  +1.14%
insertLookupWithKey empty   1.023e-3  1.021e-3  -0.21%
insertLookupWithKey update  2.471e-3  2.481e-3  +0.40%
insertWith empty            2.685e-4  2.753e-4  +2.52%
insertWith update           1.189e-3  1.227e-3  +3.24%
insertWith' empty           2.756e-4  2.798e-4  +1.52%
insertWith' update          1.040e-3  1.069e-3  +2.76%
insertWithKey empty         2.660e-4  2.703e-4  +1.59%
insertWithKey update        1.186e-3  1.222e-3  +3.06%
insertWithKey' empty        2.699e-4  2.745e-4  +1.70%
insertWithKey' update       1.032e-3  1.066e-3  +3.36%
lookup                      2.947e-4  3.192e-4  +8.34%
map                         0.812e-4  0.810e-4  -0.25%
mapMaybe                    1.309e-4  1.381e-4  +5.48%
mapMaybeWithKey             1.309e-4  1.382e-4  +5.55%
mapWithKey                  0.906e-4  0.908e-4  +0.17%
minView                     0.648e-7  0.562e-7 -13.37%
update                      0.916e-3  0.969e-3  +5.73%
updateLookupWithKey         1.566e-3  1.621e-3  +3.48%
Geometric mean              1.858e-4  1.885e-4  +1.47%
```
