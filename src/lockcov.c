/* impl.c.lockcov
 *                     LOCK COVERAGE TEST
 *
 * $HopeName: !lockcov.c(MM_dylan_sunflower.1) $
 */

#include "mpm.h"

int main(void){
  LockStruct a,b;

  LockInit(&a);
  LockInit(&b);
  LockClaim(&a);
  LockClaimRecursive(&b);
  LockReleaseMPM(&a);
  LockClaimRecursive(&b);
  LockFinish(&a);
  LockReleaseRecursive(&b);
  LockReleaseRecursive(&b);
  LockFinish(&b);
  LockInit(&a);
  LockClaim(&a);
  LockClaimRecursive(&a);
  LockReleaseRecursive(&a);
  LockReleaseMPM(&a);
  LockFinish(&a);

  return 0;
}
