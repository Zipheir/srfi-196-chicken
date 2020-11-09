(import scheme)
(import (chicken base))
(import (chicken platform))
(import (only r7rs define-library))

(register-feature! 'srfi-196)

(include "srfi/196.sld")
