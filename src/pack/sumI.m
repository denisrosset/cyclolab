function res = sumI(vec)
i = com.faacets.gluon.Interface.compile({
    'import com.faacets.gluon._'
    'Interface[Seq[Double], Double]("sum", "seq") {'
    'seq => seq.sum'
    '}'
                   });
res = i.call(vec);
