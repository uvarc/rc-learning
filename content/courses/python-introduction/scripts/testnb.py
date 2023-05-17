import pytest
from testbook import testbook

@pytest.fixture(scope='module')
def tb():
    with testbook('./testnb.ipynb', execute=True) as tb:
        yield tb

def test_func(tb):
   func = tb.ref("func")
#   assert func(2) == 3
# Intentional failure
   assert func(2) == 4

def test_func2(tb):
    func2 = tb.ref("func2")
#    assert func2(1, 3) == 4
# Intentional failure
    assert func2(1, 3) == 8
