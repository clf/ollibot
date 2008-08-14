(setq a-overlay (make-overlay 1 5))
(setq b-overlay (make-overlay 5 9))
(overlay-put a-overlay 'face '((foreground-color . "red")))
(overlay-put b-overlay 'face '((foreground-color . "blue")))
