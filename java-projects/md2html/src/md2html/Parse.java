package md2html;

class Parse {
    private StringBuilder input;

    Parse(StringBuilder input) {
        this.input = input;
    }

    private boolean isHeader(StringBuilder s) {
        int i = 0;
        while (i < s.length() && s.charAt(i) == '#') {
            i++;
        }
        return i != 0 && s.charAt(i) == ' ' && i < s.length();
    }

    public void toHtml(StringBuilder current) {
        if (isHeader(input)) {
            new Header(input).toHtml(current);
        } else {
            new Paragraph(input).toHtml(current);
        }
    }
}
