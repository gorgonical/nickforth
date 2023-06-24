SECTIONS
{
.text : {
      *(.pretext);
      *(.text);
}

.rodata : {
        *(.rodata);
}

.data : {
      *(.data);
}

.bss : {
     *(.bss);
}

}
