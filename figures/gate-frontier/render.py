"""Render actual Bitsailor frontier snapshots as SVG, PDF and PNG.

Uses ReportLab for vector drawing and pdftoppm for PNG conversion. See README.md.
"""
import json
import math
import subprocess
from pathlib import Path
from reportlab.graphics.shapes import Drawing, Group, Rect, Line, String, Circle
from reportlab.graphics import renderSVG, renderPDF
from reportlab.lib.colors import HexColor, Color
from reportlab.pdfgen.canvas import Canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
import reportlab

FONT_DIR = Path('/usr/share/fonts/liberation-sans-fonts')
if FONT_DIR.exists():
    regular, bold = FONT_DIR / 'LiberationSans-Regular.ttf', FONT_DIR / 'LiberationSans-Bold.ttf'
else:
    fonts = Path(reportlab.__file__).resolve().parent / 'fonts'
    regular, bold = fonts / 'Vera.ttf', fonts / 'VeraBd.ttf'
pdfmetrics.registerFont(TTFont('FrontierSans', str(regular)))
pdfmetrics.registerFont(TTFont('FrontierSans-Bold', str(bold)))

ROOT = Path(__file__).resolve().parent
FRAMES = {seconds // 60: (seconds, bins, states) for seconds, bins, states in json.loads((ROOT / 'frontiers.json').read_text())}
PANELS = [
    (14, 'Approaching the raised gate', 'The direct route passes south of the entire gate.'),
    (18, 'First crossings', 'Both stages advance on the same clock.'),
    (30, 'Alternative approaches', 'Later crossings remain in competition.'),
    (44, 'Coverage increases', '4 of 12 intervals reached; both stages still active.'),
    (52, 'Pre-gate stage retires', 'Coverage reaches 6/12: the 50% threshold.'),
    (54, 'Continuing to destination', 'Only histories that crossed the gate remain.'),
]

KM = 111.195  # Local equatorial projection, used for illustration only.
BG, INK, MUTED = '#fcfaf6', '#213039', '#667780'
BLUE, ORANGE, GREEN = '#26759c', '#d75b3d', '#329785'


def text(d, x, y, value, size=12, color=INK, bold=False):
    d.add(String(x, y, str(value), fontName='FrontierSans-Bold' if bold else 'FrontierSans', fontSize=size, fillColor=HexColor(color)))


def rect(d, x, y, w, h, fill, stroke=None):
    d.add(Rect(x, y, w, h, fillColor=HexColor(fill), strokeColor=HexColor(stroke) if stroke else None))


def plot(d, minute, ox, oy, scale, radius=1.25):
    seconds, bins, states = FRAMES[minute]
    def xy(x, y):
        return ox + (x + 15) * scale, oy + (y + 12) * scale
    size = 32 * scale
    rect(d, ox, oy, size, size, '#ffffff', '#cdd6d9')
    for x in [-12, -6, 0, 6, 12]:
        px, _ = xy(x, 0)
        d.add(Line(px, oy, px, oy + size, strokeColor=HexColor('#e8edef'), strokeWidth=.5))
        text(d, px - 3, oy - 17, x, 9, MUTED)
    for y in [-8, 0, 8, 16]:
        _, py = xy(0, y)
        d.add(Line(ox, py, ox + size, py, strokeColor=HexColor('#e8edef'), strokeWidth=.5))
        text(d, ox - 24, py - 3, y, 9, MUTED)
    # The direct start-destination line misses the shifted gate completely.
    sx, sy = xy(0, 0)
    dx, dy = xy(.14 * KM, 0)
    d.add(Line(sx, sy, dx, dy, strokeColor=HexColor('#89969d'), strokeWidth=.9, strokeDashArray=[4, 3]))
    centres = [(0, 0), (.04 * KM, .07 * KM)]
    chosen = []
    for stage in [0, 1]:
        points = [point for point in states if point[0] == stage]
        cx, cy = centres[stage]
        ordered = sorted(points, key=lambda p: math.atan2(p[2] * KM - cy, p[1] * KM - cx))
        indices = sorted(set(int(q * (len(ordered) - 1)) for q in [.12, .5, .88])) if ordered else []
        for index in indices:
            point = ordered[index]
            st, lon, lat, cross_lon, cross_lat = point
            px, py = xy(lon * KM, lat * KM)
            rx, ry = xy(cx, cy)
            d.add(Line(rx, ry, px, py, strokeColor=HexColor(['#80a9bc', '#e5a28f'][stage]), strokeWidth=.9))
            if stage == 1:
                ax, ay = xy(cross_lon * KM, cross_lat * KM)
                d.add(Line(ax, ay, px, py, strokeColor=HexColor('#677b72'), strokeWidth=.9, strokeDashArray=[3, 2]))
                d.add(Circle(ax, ay, 2.3, fillColor=HexColor('#ffffff'), strokeColor=HexColor(GREEN), strokeWidth=.9))
            chosen.append((stage, px, py))
    for stage in [0, 1]:
        for st, lon, lat, *_origin in states:
            if st != stage:
                continue
            px, py = xy(lon * KM, lat * KM)
            d.add(Circle(px, py, radius, strokeColor=None, fillColor=HexColor([BLUE, ORANGE][stage])))
    for stage, px, py in chosen:
        d.add(Circle(px, py, 2.8, fillColor=HexColor('#ffffff'), strokeColor=HexColor([BLUE, ORANGE][stage]), strokeWidth=1.2))
    gx, _ = xy(.04 * KM, 0)
    bottom, top = .02 * KM, .12 * KM
    for i, hit in enumerate(bins):
        _, y0 = xy(0, bottom + (top - bottom) * i / 12)
        _, y1 = xy(0, bottom + (top - bottom) * (i + 1) / 12)
        d.add(Line(gx, y0, gx, y1, strokeColor=HexColor(GREEN if hit else '#b8c4c7'), strokeWidth=4))
        d.add(Line(gx - 3, y0, gx + 3, y0, strokeColor=HexColor('#ffffff'), strokeWidth=.8))
    _, gy = xy(0, top)
    text(d, gx - 12, gy + 10, 'Gate', 10, INK, True)
    for x, label in [(0, 'Start (S)'), (.14 * KM, 'Destination')]:
        px, py = xy(x, 0)
        d.add(Circle(px, py, 3.5, fillColor=HexColor(INK), strokeColor=HexColor('#ffffff'), strokeWidth=1))
        text(d, px - (18 if x == 0 else 26), py - 17, label, 9)
    _, mid = xy(0, .07 * KM)
    d.add(Line(gx - 4, mid, gx + 4, mid, strokeColor=HexColor(INK), strokeWidth=1))
    d.add(Line(gx, mid - 4, gx, mid + 4, strokeColor=HexColor(INK), strokeWidth=1))
    text(d, gx + 7, mid + 5, 'G', 11, INK, True)
    text(d, ox + size / 2 - 54, oy - 36, 'East from start (km)', 10, MUTED)
    group = Group(transform=(0, 1, -1, 0, ox - 40, oy + size / 2 - 50))
    text(group, 0, 0, 'North from start (km)', 10, MUTED)
    d.add(group)
    return bins, [sum(point[0] == stage for point in states) for stage in [0, 1]]


def legend(d, x, y, compact=False):
    for dx, color, label in [(0, BLUE, '0 gates crossed'), (190, ORANGE, '1 gate crossed'), (365, GREEN, 'Gate intervals reached')]:
        rect(d, x + dx, y - 2, 12, 5, color)
        text(d, x + dx + 20, y - 3, label, 11)


pdf = Canvas(str(ROOT / 'gate-frontier-sequence.pdf'), pagesize=(1000, 740))
pdf.setTitle('Bitsailor: search-frontier progression through a gate')
for index, (minute, title, caption) in enumerate(PANELS, 1):
    drawing = Drawing(1000, 740)
    rect(drawing, 0, 0, 1000, 740, BG)
    text(drawing, 54, 697, f'{index:02d}  /  {title}', 27, bold=True)
    text(drawing, 54, 668, f'T + {minute:02d} min   |   {caption}', 15, MUTED)
    legend(drawing, 54, 632)
    bins, counts = plot(drawing, minute, 80, 108, 15.1875, 1.5)
    text(drawing, 635, 558, 'ACTIVE FRONTIER', 12, MUTED, True)
    text(drawing, 635, 519, f'{counts[0]:,} states before gate', 20, BLUE, True)
    text(drawing, 635, 486, f'{counts[1]:,} states after crossing', 20, ORANGE, True)
    text(drawing, 635, 427, 'GATE COVERAGE', 12, MUTED, True)
    for i, hit in enumerate(bins):
        rect(drawing, 635 + 23 * i, 395, 19, 16, GREEN if hit else '#dce3e4')
    text(drawing, 635, 371, f'{sum(bins)} / 12 distinct intervals reached', 15, bold=True)
    text(drawing, 635, 345, 'Retirement requires at least 6 / 12 (50%).', 12, MUTED)
    text(drawing, 635, 297, 'PRE-GATE STAGE', 12, MUTED, True)
    text(drawing, 635, 269, 'RETIRED' if counts[0] == 0 else 'ACTIVE', 21, GREEN if not counts[0] else BLUE, True)
    for j, line in enumerate(['Solid rays: fixed reference to state.', 'Blue starts at S; orange starts at G.', 'Dashed green: actual crossing to state.', 'The search fan uses the actual crossing;', 'buckets and radial pruning use S/G.', '', 'Hollow circles mark selected states', 'and their gate-crossing origins.']):
        text(drawing, 635, 229 - 17 * j, line, 12, MUTED)
    text(drawing, 54, 45, 'Actual Bitsailor search snapshots • constant 5 m/s boat speed • constant northerly wind • no land', 11, MUTED)
    text(drawing, 54, 26, 'Origin rays show selected states. All panels share the same scale; rays are not sailed tracks.', 11, MUTED)
    stem = f'{index:02d}-{minute:02d}min'
    renderSVG.drawToFile(drawing, str(ROOT / f'{stem}.svg'))
    renderPDF.draw(drawing, pdf, 0, 0)
    pdf.showPage()
pdf.save()
subprocess.run(['pdftoppm', '-png', '-r', '130', str(ROOT / 'gate-frontier-sequence.pdf'), str(ROOT / 'frame')], check=True)
for index, (minute, *_rest) in enumerate(PANELS, 1):
    (ROOT / f'frame-{index}.png').rename(ROOT / f'{index:02d}-{minute:02d}min.png')

# A single page for comparing all times without changing the map scale.
overview = Drawing(1360, 930)
rect(overview, 0, 0, 1360, 930, BG)
text(overview, 42, 884, 'How the search frontier progresses through a gate', 27, bold=True)
text(overview, 42, 855, 'Fan from actual crossing • bucket reference remains S/G • solid rays: bucket origin; dashed green: crossing origin', 15, MUTED)
legend(overview, 42, 824)
for index, (minute, title, _caption) in enumerate(PANELS):
    col, row = index % 3, index // 3
    x, y = 70 + col * 445, 467 - row * 385
    text(overview, x - 18, y + 307, f'{index + 1:02d}  T+{minute:02d} min · {title}', 12.5, bold=True)
    bins, counts = plot(overview, minute, x, y, 7.9875, .8)
    text(overview, x, y + 280, f'Before: {counts[0]:,}   After: {counts[1]:,}   Coverage: {sum(bins)}/12', 11, MUTED)
text(overview, 42, 22, 'Actual controlled Bitsailor run • no land • same scale in all panels • gate shifted north of the direct route • rays are not sailed paths', 11, MUTED)
renderSVG.drawToFile(overview, str(ROOT / 'gate-frontier-overview.svg'))
renderPDF.drawToFile(overview, str(ROOT / 'gate-frontier-overview.pdf'))
subprocess.run(['pdftoppm', '-singlefile', '-png', '-r', '130', str(ROOT / 'gate-frontier-overview.pdf'), str(ROOT / 'gate-frontier-overview')], check=True)
print('Rendered six SVG/PNG figures, a six-page PDF and an overview in', ROOT)
