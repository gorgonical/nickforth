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

.end.data : {
     *(.end.data);
}

}
