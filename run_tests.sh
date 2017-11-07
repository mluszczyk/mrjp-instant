set -xe

TEST_TEMPLATE="${TMPDIR}instantXXX"
TEST_DIR=`mktemp -d "$TEST_TEMPLATE"`

for input_file in examples/*.ins; do
  BASENAME=`basename "$input_file" .ins`
  LLFILE="$TEST_DIR/${BASENAME}.ll"
  JFILE="$TEST_DIR/${BASENAME}.j"
  CLANG_OUT="$TEST_DIR/${BASENAME}.out"
  JAVA_CLASS="$TEST_DIR/${BASENAME}.class"
  LLVM_ANS="$TEST_DIR/${BASENAME}.llans"
  JVM_ANS="$TEST_DIR/${BASENAME}.jasn"
  CORRECT_ANS="examples/${BASENAME}.output"
  stack exec compile llvm "$input_file" > $LLFILE
  clang "$LLFILE" -o "$CLANG_OUT"
  $CLANG_OUT > $LLVM_ANS
  diff -q $CORRECT_ANS $LLVM_ANS
  stack exec compile jvm "$input_file" > $JFILE
  sed -i '' "s/Instant/${BASENAME}/1" "$JFILE"
  jasmin -d "$TEST_DIR" "$JFILE"
  java -cp "$TEST_DIR" $BASENAME  > $JVM_ANS
  diff -q $CORRECT_ANS $JVM_ANS
done

# rm -rf "$TEST_DIR"
