test('Chan001',      reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])

test('Chan002',      extra_run_opts('100'), compile_and_run, [''])
test('Chan003',      extra_run_opts('200'), compile_and_run, [''])

test('MVar001',      reqlib('QuickCheck'), compile_and_run, ['-package QuickCheck'])
test('ThreadDelay001', normal, compile_and_run, [''])

test('stmioA02', normal, compile_and_run, [''])
test('stmioA03', normal, compile_and_run, [''])
test('stmio047', normal, compile_and_run, [''])
test('stmio049', reqlib('random'), compile_and_run, [''])
test('stmio052', reqlib('random'), compile_and_run, [''])
test('stmio060', normal, compile_and_run, [''])
test('stmio060A', normal, compile_and_run, [''])
test('stmio061', normal, compile_and_run, [''])
test('stmio_masking', normal, compile_and_run, [''])
