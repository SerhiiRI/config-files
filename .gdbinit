set prompt (\033[01;92mgdb\033[0m) 

define startQemu
  if $argc == 1
    shell echo -e "\033[92mStarting localhost kernel\033[0m"
    shell qemu-system-i386 -kernel $arg0 -s -S &
  end
  if $argc == 0
    shell echo -e "\033[31mEnter path to kernel\033[0m"
  end
end


define killQemu
  shell kill -KILL (pgrep qemu)
end


define connectToKernel
  target remote localhost:1234
end


define sleepgdb
  while(1)
    shell sleep 1
    step
  end
end
