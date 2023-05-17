import pytest
import datetime
from dow import DoW

# This ordering for datetime
days=["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]

dates=[(30,5,2016),(14,2,2000),(4,7,1971),(4,7,1776),(1,9,2016),(1,9,2000)]
test_input=[]
expecteds=[]
for d,m,y in dates:
    day_number = datetime.date(year=y, month=m, day=d).weekday()
    expecteds.append(days[day_number])
    test_input.append(DoW(d,m,y))
tests=[]
for i,test in enumerate(test_input):
    tests.append((test,expecteds[i]))

@pytest.mark.parametrize("test_input,expected",tests)

def test_eval(test_input,expected):
    assert test_input==expected

