(in-package :ad-exe)

;; registers the process modules aliases for this executive
;; For now, switching to PR2 robot. But we should find an elegant way to switch between Jido and PR2

;; PMs for Jido robot:
;(cpm:process-module-alias :ptu 'mjido-pm:morse-jido-ptu-process-module)
;(cpm:process-module-alias :manipulation 'mjido-pm:morse-jido-manip-process-module)
;; (cpm:process-module-alias :navigation 'mjido-pm:morse-jido-navigation-process-module)
;; (cpm:process-module-alias :perception 'fake-process-modules:fake-perception)

;; PMs for PR2 robot:
;; Fake-PMs:
(cpm:process-module-alias :ptu 'fake-process-modules:fake-ptu)
(cpm:process-module-alias :manipulation 'fake-process-modules:fake-manipulation)
(cpm:process-module-alias :perception 'fake-process-modules:fake-perception)
;; Real PMs: (todo: replace above with real PMs)
(cpm:process-module-alias :navigation 'mpr2-pm:morse-pr2-navigation-process-module)