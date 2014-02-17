lambda-calculus-evaluator
=========================

To try this out by adding 1 and 3 run

    JAVA_OPTS="-Dfile.encoding=utf8" scala lambda.scala "((Lx.Ly.La.Lb.((x a) ((y a) b)) Lx.Ly.(x y)) Lx.Ly.(x (x (x y))))"
