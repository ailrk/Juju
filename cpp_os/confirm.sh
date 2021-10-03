if grub-file --is-x86-multiboot os.bin; then
    echo multiboot conformed
else
    echo "thie file is not multiboot"
fi
