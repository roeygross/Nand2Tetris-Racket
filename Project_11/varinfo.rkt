; varinfo

; provides variable info

#lang racket/base
(provide varInfo-name)
(provide varInfo-type)
(provide varInfo-kind)
(provide varInfo-index)
(provide varInfo-scope)
(provide varInfo)

;varInfo 
(struct varInfo (name type kind index scope))
