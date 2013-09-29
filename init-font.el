(when *is-cocoa-emacs*
  (create-fontset-from-fontset-spec
   "-*-Menlo-Regular-r-*-*-15-*-*-*-m-*-fontset-term13,
ascii:-*-Menlo-Regular-r-*-*-15-*-*-*-m-*-fontset-term13")
  (set-face-font 'default "fontset-term13")
  (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                    "-*-Apple SD Gothic Neo-Regular-r-*-*-15-*-*-*-m-*-fontset-term13"))

(provide 'init-font)
