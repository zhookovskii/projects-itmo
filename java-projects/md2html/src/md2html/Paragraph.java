package md2html;

class Paragraph {
    private StringBuilder input;

    Paragraph(StringBuilder input) {
        this.input = input;
    }

    public void toHtml(StringBuilder current) {
        current.append("<p>");
        new Text(input).toHtml(current);
        current.append("</p>");
    }
}
