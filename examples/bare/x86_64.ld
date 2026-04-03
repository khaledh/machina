ENTRY(kmain)

PHDRS
{
    text PT_LOAD FLAGS(5);
    data PT_LOAD FLAGS(6);
}

SECTIONS
{
    . = 1M;

    .text : ALIGN(16) {
        *(.text)
        *(.text.*)
    } :text

    .rodata : ALIGN(16) {
        *(.rodata)
        *(.rodata.*)
    } :text

    .data : ALIGN(16) {
        *(.data)
        *(.data.*)
    } :data

    .bss : ALIGN(16) {
        *(.bss)
        *(.bss.*)
        *(COMMON)
    } :data

    /DISCARD/ : {
        *(.note.GNU-stack)
    }
}
