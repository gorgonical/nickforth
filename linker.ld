SECTIONS
{
.text : {
      *(.pretext);
      *(.text);
}

.bss : {
     *(.bss);
}

.rodata : {
        *(.rodata);
}

.data : {
      *(.data);
}

}
