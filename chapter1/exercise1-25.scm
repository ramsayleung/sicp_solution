;;; 这个自己就没找出原因，按照 http://community.schemewiki.org/?sicp-ex-1.25 的
;;; 解释，改进的 fast-expt 速度会变慢，是因为 fast-expt 计算会产生非常大的中间值，虽说
;;; scheme 可以计算任意精度的数字，但是太长的数字还是会需要耗费的资源。而 原来的
;;; 计算方法就可以使计算期间产生的值都会小于 (expmod base exp m) 中的 m
