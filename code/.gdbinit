handle SIGXFSZ nostop noprint
handle SIGXCPU nostop noprint
handle SIGSEGV nostop noprint
define rabbit
  while 1
    run > /dev/null
    if $_siginfo
      loop_break
    end
  end
end
set print thread-events off
