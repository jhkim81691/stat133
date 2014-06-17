import rpy2.robjects as ro

# Add parent directory to path, so check will import
import sys
sys.path.append('..')
from check import start_log, check, run, save_grades

lab = "ex1"
language = "r"
possible = 8
record = False  # turn on when ready to record grades
g = globals()

score = start_log(lab, 4, possible, g, language)

r_source = ro.r['source']
try:
    m = r_source(lab+'.r')
except:
    pass

#run('sd = ro.globalenv["mySD"]', g)
score += check("ro.r('mySD(c(9,5,2,3,7))')[0]", 2.8635642126552705, 2, g)
#score += check("ro.r(\"all.equal()\")[0]", True, 2, g)
score += check("ro.r('myMean(c(9,5,2,3,7))')[0]", 5.2, 2, g)

if record:
    save_grades(lab, score, possible)
else:
    print '='*17
    print 'Not recording ...'
    print '='*17
    print 'Score: ' + str(score) 
    print 'Out of: ' + str(possible) 
    print '='*17
