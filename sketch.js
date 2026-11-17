// =============================================================
// マウスのドラッグで引いた線の上を、tile画像が流れるベルトコンベア風スケッチ
//
// 構成:
//   1. ドラッグ中のマウス座標を一定間隔でサンプリングして1本のトラック(折れ線)にする
//   2. トラックごとに専用のマスク・円バッファを持つ(他の線の領域にはみ出さない)
//      - 描いている最中の線: キャンバス全面サイズの共有バッファを使い回す
//      - 描き終わった線: バウンディングボックス分だけの小さいバッファを1回だけ作る
//   3. 円の間隔は CIRCLE_DIAMETER + CIRCLE_GAP の固定値。
//      開いたパスなので無限に続くコンベアとして扱う
//   4. ピース(流すタイル)は右パネルの箱に画像またはSVGをドロップして登録する。
//      SVGピースは、画面用にラスター化しつつ元のSVGも保持し、Export SVGで使う
//
// 操作: ドラッグで線を描く / z キーで1本戻す / c キーで全消去
// =============================================================

let pieces = [];       // ピース画像(p5.Image)。null は画像を入れていない空の箱
let tintedPieces = []; // circleColorでtint(色付け)済みの生Canvas(piecesと同じ並び。buildTintedImagesで生成)
let pieceMeta = [];    // piecesと同じ並び。SVGピースなら { svgText, viewBox }、PNGなどのラスター画像・空の箱は null
let tracks       = []; // 各線のデータ { pts, segLens, total, ... }
let trackGroupId = 0;    // SVGパスをまとめて読み込んだ線に付ける番号(Undoで一度に戻すため)
let activeTrack  = null; // ドラッグ中の線(なければnull)
let baseImg;        // 今のモードの背景画像。キャンバスいっぱいに敷く
let baseImgs = {};  // モードごとの背景画像 { base, insta }
let canvasMode;     // 今のキャンバスモード('base' | 'insta')

// --- キャンバスサイズ ---
// モードごとのキャンバスサイズ・背景画像・初期パス(SVG)
const CANVAS_MODES = {
  blank: { w: 960, h: 640,  img: null,        path: null },        // 画像もパスも読み込まない真っ白モード
  base:  { w: 960, h: 640,  img: 'base.jpg',  path: 'base.svg' },
  insta: { w: 756, h: 945,  img: 'insta.jpg', path: 'insta.svg' },
  session: { w: 756, h: 945, img: 'session.jpg', path: 'session.svg' }, // talk session
  workshop: { w: 756, h: 945, img: 'workshop.jpg', path: 'workshop.svg' },
  crosstalk: { w: 756, h: 945, img: 'crosstalk.jpg', path: 'crosstalk.svg' },
};

// --- 見た目・動きの調整パラメータ ---
// 値の本体は PARAMS(Tweakpaneのスライダーが書き換える)。
// 毎フレーム syncParams() で下のグローバル変数へ写して使う。
const PARAMS = {
  canvasMode:     'base', // キャンバスのサイズと背景画像: 'base'(960×640) | 'insta'(756×945) | 'session'(756×945) | 'workshop'(756×945) | 'crosstalk'(756×945)
  circleDiameter: 70,    // 円の直径(px)
  circleGapRatio: -0.6, // 円と円の間隔(円の直径に対する比率)。0なら隙間なしでぴったり並ぶ。-1に近いほど重なる
  beltSpeed:      0.4,   // 1フレームあたりの移動量(px)
  trackWidth:     400,    // トラック(ストローク)の太さ
  minPointDist:   50,     // ドラッグ中、前の点からこの距離以上動いたら新しい点を追加する(px)
  jitterAmount:   0.0,     // レール(進行方向)に垂直な向きへのずれの最大値(px)。0でずれなし
  ampLoop:        true, // true: amountが 設定値→最大値→設定値 をループする
  ampMax:         200,   // ループの最大値(px)
  ampSec:         3,     // 1往復(設定値→最大値→設定値)にかける秒数
  ampFps:         6,    // amountループのコマ数(1秒あたり)。小さいほどカクカク動く
  ampWait:        3,     // 1往復のあと、設定値のまま待つ秒数(インターバル)
  randomSeed:     413,     // ランダム配置の乱数の種(変えると並びが変わる)
  randomOrder:    true, // true: ピースを交互ではなくランダムな順番で配置する
  rotLoop:        true, // true: 回転が 最小→最大→(静止)→最小→(静止) をループする
  rotMin:         0,  // ループの最小値(度)
  rotMax:         180,   // ループの最大値(度)
  rotFps:         6,    // 回転ループのコマ数(1秒あたり)。小さいほどカクカク動く(コマ撮り風)
  rotMoveSec:     2,     // 最小↔最大の移動にかける秒数
  pieceRotation:  -180,     // 全ピースの向きを、進行方向からこの角度(度)だけ一律に回す。0で回転なし
  zoneScale:      0.01,   // 「揺れる区間/揺れない区間」の粗さ。小さいほど長い区間で切り替わる
  zoneThreshold:  0,   // このゾーンノイズ値を境に揺れON/OFFが切り替わる(0〜1)
  zoneSoftness:   0.001,  // ON/OFFの境界のなめらかさ。0に近いほどくっきりコントラストが出る
  showTrack:      false,  // トラックの白い縁取りを表示するか
  fillPath:       false,  // 読み込んだSVGのパスを、始点と終点をつないで閉じ、中を塗りつぶすか
  fillColor:      "#ffffff", // 塗りつぶしの色
  showImage:      true,  // 背景画像(base.jpg)を表示するか
  bkgdColor:      "#ffffff",
  trackColor:     "#ffffff",
  circleColor:    "#ffffff",
};

const DEFAULT_PARAMS = { ...PARAMS }; // Reset用の初期値(localStorage復元より前に控える)

const CURVE_STEPS = 32;   // SVGパス読み込み用:曲線1本あたりの分割数
const SIMPLIFY_EPSILON = 0.5; // SVG書き出し時、線の点を間引く許容誤差(px)
const MAX_PIECES = 8;     // ピースの箱の最大数
const MAX_PIECE_PX = 256; // ピース画像の長辺の上限(px)。これを超える画像は読み込み時に縮小する

let CIRCLE_DIAMETER, CIRCLE_GAP, BELT_SPEED, TRACK_WIDTH, MIN_POINT_DIST;
let RANDOM_ORDER, RANDOM_SEED, JITTER_AMOUNT, PIECE_ROTATION, ZONE_SCALE, ZONE_THRESHOLD, ZONE_SOFTNESS, SHOW_TRACK, SHOW_IMAGE, FILL_PATH, FILL_COLOR;
let bkgdColor, trackColor, circleColor; // bkgdColorはUSE_TRANSPARENT_BG=trueの場合は使われない

const USE_TRANSPARENT_BG = false; // 背景を透明にするか(シール表示用)

const STORAGE_KEY = 'conveyor-brush-params-v2';
const OLD_STORAGE_KEY = 'conveyor-brush-params'; // 以前の形式(モードごとに分かれていない)
let pane;

// スライダーの値は、キャンバスのモードごとに別々に持つ(canvasMode 自体は共通)。
// modeParams[モード名] = そのモードのスライダーの値
const modeParams = {};

// モードごとの値を PARAMS へ反映する。
// そのモードの保存がまだなければ、今の値をそのまま引き継ぐ(切り替えただけで初期化しない)。
// 保存のある項目だけ入れ替え、型が違う・足りない項目は初期値にする
function applyModeParams(name) {
  const saved = modeParams[name];
  if (!saved) return;
  for (const key of Object.keys(DEFAULT_PARAMS)) {
    if (key === 'canvasMode') continue;
    PARAMS[key] = typeof saved[key] === typeof DEFAULT_PARAMS[key] ? saved[key] : DEFAULT_PARAMS[key];
  }
}

// 今の PARAMS(canvasMode 以外)を、モード name の値として控える
function snapshotParams(name) {
  const snap = {};
  for (const key of Object.keys(PARAMS)) if (key !== 'canvasMode') snap[key] = PARAMS[key];
  modeParams[name] = snap;
}

function loadParams() {
  try {
    let saved = JSON.parse(localStorage.getItem(STORAGE_KEY));
    if (!saved) {
      // 以前の形式の保存があれば、そのときのモードの値として引き継ぐ
      const old = JSON.parse(localStorage.getItem(OLD_STORAGE_KEY));
      if (old) {
        const m = CANVAS_MODES[old.canvasMode] ? old.canvasMode : 'base';
        saved = { canvasMode: m, modes: { [m]: old } };
      }
    }
    if (!saved) return;
    Object.assign(modeParams, saved.modes || {});
    if (CANVAS_MODES[saved.canvasMode]) PARAMS.canvasMode = saved.canvasMode;
    applyModeParams(PARAMS.canvasMode);
  } catch (e) {}
}

let switchingMode = false; // モード切り替え中(refreshで発火する 'change' による保存を止める)

function saveParams() {
  if (switchingMode) return;
  try {
    // 今のモードの値を控えてから、全モード分をまとめて保存する
    // (モードの切り替え直後は、まだ前のモードの値が PARAMS に入っているので、canvasMode(前のモード)のキーで控える)
    snapshotParams(canvasMode || PARAMS.canvasMode);
    localStorage.setItem(STORAGE_KEY, JSON.stringify({ canvasMode: PARAMS.canvasMode, modes: modeParams }));
  } catch (e) {}
}

// 回転のループ: 最小→最大(移動)→2秒静止→最大→最小(移動)→2秒静止 を繰り返す
const ROT_HOLD_MS = 2000;
function loopedRotation() {
  const move = Math.max(1, PARAMS.rotMoveSec * 1000);
  // コマ数を減らすため、時間をコマ(1000/rotFps ms)の単位に切り捨ててから角度を求める
  const frame = 1000 / Math.max(1, PARAMS.rotFps);
  const t = Math.floor(millis() / frame) * frame % (2 * (move + ROT_HOLD_MS));
  const a = PARAMS.rotMin, b = PARAMS.rotMax;
  if (t < move) return a + (b - a) * (t / move);                    // 最小→最大
  if (t < move + ROT_HOLD_MS) return b;                              // 最大で静止
  if (t < 2 * move + ROT_HOLD_MS) return b + (a - b) * ((t - move - ROT_HOLD_MS) / move); // 最大→最小
  return a;                                                          // 最小で静止
}

// amountのループ: 設定値→最大値→設定値 を ampSec 秒かけて1往復し、ampWait 秒待ってから繰り返す
function loopedAmount() {
  const move = Math.max(0.1, PARAMS.ampSec) * 1000;
  const wait = Math.max(0, PARAMS.ampWait) * 1000;
  const frame = 1000 / Math.max(1, PARAMS.ampFps); // コマ数を減らすため、時間をコマ単位に切り捨てる
  const t = Math.floor(millis() / frame) * frame % (move + wait);
  if (t >= move) return PARAMS.jitterAmount;         // インターバル中は設定値のまま
  const k = 0.5 - 0.5 * Math.cos((t / move) * 2 * Math.PI); // 0→1→0
  return PARAMS.jitterAmount + (PARAMS.ampMax - PARAMS.jitterAmount) * k;
}

// PARAMS -> グローバル変数へ反映。太さ・色が変わったときだけバッファを作り直す
function syncParams() {
  if (canvasMode && PARAMS.canvasMode !== canvasMode) applyCanvasMode(PARAMS.canvasMode);
  const colorChanged = PARAMS.circleColor !== circleColor;

  CIRCLE_DIAMETER = PARAMS.circleDiameter;
  CIRCLE_GAP      = PARAMS.circleGapRatio * PARAMS.circleDiameter; // 直径に比例した間隔(px)
  BELT_SPEED      = PARAMS.beltSpeed;
  TRACK_WIDTH     = PARAMS.trackWidth;
  MIN_POINT_DIST  = PARAMS.minPointDist;
  RANDOM_ORDER    = PARAMS.randomOrder;
  RANDOM_SEED     = PARAMS.randomSeed;
  JITTER_AMOUNT   = PARAMS.ampLoop ? loopedAmount() : PARAMS.jitterAmount;
  PIECE_ROTATION = PARAMS.rotLoop ? loopedRotation() : PARAMS.pieceRotation;
  ZONE_SCALE      = PARAMS.zoneScale;
  ZONE_THRESHOLD  = PARAMS.zoneThreshold;
  ZONE_SOFTNESS   = PARAMS.zoneSoftness;
  SHOW_TRACK      = PARAMS.showTrack;
  SHOW_IMAGE      = PARAMS.showImage;
  FILL_PATH       = PARAMS.fillPath;
  FILL_COLOR      = PARAMS.fillColor;
  bkgdColor       = PARAMS.bkgdColor;
  trackColor      = PARAMS.trackColor;
  circleColor     = PARAMS.circleColor;

  if (colorChanged && pieces.some(Boolean)) buildTintedImages();
}

// ------------------------------------------------------------
// 調整パネル(Tweakpane)
// ------------------------------------------------------------
function setupPane() {
  pane = new Tweakpane.Pane({
    title: 'PLAY with Maker',
    container: document.getElementById('pane-holder')
  });

  const canvasFolder = pane.addFolder({ title: 'Canvas' });
  canvasFolder.addBinding(PARAMS, 'canvasMode', {
    label: 'mode',
    options: { 'blank (960×640)': 'blank', 'base (960×640)': 'base', 'insta (756×945)': 'insta', 'talk session (756×945)': 'session', 'workshop (756×945)': 'workshop', 'crosstalk (756×945)': 'crosstalk' }
  });

  const tile = pane.addFolder({ title: 'Piece' });
  tile.addBinding(PARAMS, 'circleDiameter', { label: 'piece size', min: 1, max: 100, step: 1 });
  tile.addBinding(PARAMS, 'circleGapRatio', { label: 'piece gap', min: -0.99, max: 0, step: 0.001 });
  tile.addBinding(PARAMS, 'randomOrder', { label: 'random order' });
  tile.addBinding(PARAMS, 'randomSeed', { label: 'seed', min: 0, max: 999, step: 1 });
  tile.addBinding(PARAMS, 'beltSpeed', { label: 'speed', min: 0, max: 5, step: 0.05 });
  tile.addBinding(PARAMS, 'trackWidth', { label: 'track w', min: 1, max: 400, step: 1 });
  tile.addBinding(PARAMS, 'minPointDist', { label: 'point step', min: 1, max: 100, step: 1 });

  const jitter = pane.addFolder({ title: 'Jitter' });
  jitter.addBinding(PARAMS, 'jitterAmount', { label: 'amount', min: 0, max: 50, step: 0.5 });
  jitter.addBinding(PARAMS, 'ampLoop', { label: 'amount loop' });
  jitter.addBinding(PARAMS, 'ampMax', { label: 'amount max', min: 0, max: 200, step: 0.5 });
  jitter.addBinding(PARAMS, 'ampSec', { label: 'amount sec', min: 0.1, max: 30, step: 0.1 });
  jitter.addBinding(PARAMS, 'ampFps', { label: 'amount fps', min: 1, max: 60, step: 1 });
  jitter.addBinding(PARAMS, 'ampWait', { label: 'amount wait', min: 0, max: 30, step: 0.1 });
  jitter.addBinding(PARAMS, 'pieceRotation', { label: 'rotation', min: -180, max: 180, step: 1 });
  jitter.addBinding(PARAMS, 'rotLoop', { label: 'rot loop' });
  jitter.addBinding(PARAMS, 'rotMin', { label: 'rot min', min: -180, max: 180, step: 1 });
  jitter.addBinding(PARAMS, 'rotMax', { label: 'rot max', min: -180, max: 180, step: 1 });
  jitter.addBinding(PARAMS, 'rotFps', { label: 'rot fps', min: 1, max: 60, step: 1 });
  jitter.addBinding(PARAMS, 'rotMoveSec', { label: 'rot sec', min: 0.1, max: 10, step: 0.1 });
  jitter.addBinding(PARAMS, 'zoneScale', { label: 'zone scale', min: 0.01, max: 2, step: 0.01 });
  jitter.addBinding(PARAMS, 'zoneThreshold', { label: 'threshold', min: 0, max: 1, step: 0.01 });
  jitter.addBinding(PARAMS, 'zoneSoftness', { label: 'softness', min: 0.001, max: 0.5, step: 0.001 });

  const palette = pane.addFolder({ title: 'Palette' });
  palette.addBinding(PARAMS, 'showTrack', { label: 'show track' });
  palette.addBinding(PARAMS, 'showImage', { label: 'show image' });
  palette.addBinding(PARAMS, 'fillPath', { label: 'fill path' });
  palette.addBinding(PARAMS, 'fillColor', { label: 'fill color' });
  palette.addBinding(PARAMS, 'bkgdColor', { label: 'bg' });
  palette.addBinding(PARAMS, 'trackColor', { label: 'track color' });
  palette.addBinding(PARAMS, 'circleColor', { label: 'piece color' });

  pane.addButton({ title: 'Import Path SVG' }).on('click', () => {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.svg,image/svg+xml';
    input.onchange = () => importPathSvgFile(input.files[0]);
    input.click();
  });
  pane.addButton({ title: 'Reset Params' }).on('click', () => {
    Object.assign(PARAMS, DEFAULT_PARAMS, { canvasMode: PARAMS.canvasMode }); // 今のモードだけ初期値に戻す
    pane.refresh();
    saveParams();
  });
  pane.addButton({ title: 'Undo' }).on('click', undoTrack);
  pane.addButton({ title: 'Clear Canvas' }).on('click', () => { if (!activeTrack) removeAllTracks(); });

  pane.addButton({ title: 'Export SVG' }).on('click', exportSVG);
  pane.addButton({ title: 'Export PNG' }).on('click', () => saveCanvas('play-with-maker', 'png'));

  pane.on('change', saveParams);
}

let beltOffset = 0;

// ============================================================
// setup / draw
// ============================================================

// 初期ピース(SVG。Export SVGでもベクターのまま書き出される)
const DEFAULT_PIECES = ['tile.svg', 'tile2.svg', 'tile_3.svg', 'tile_4.svg'];

function loadDefaultPieces() {
  DEFAULT_PIECES.forEach((url, i) => {
    // SVGの文字列を読んでから、画面用にラスター化する(file:// などでfetchできない場合は、画像として読む)
    fetch(url)
      .then(r => { if (!r.ok) throw new Error(r.status); return r.text(); })
      .then(text => {
        const info = prepareSvgPiece(text);
        if (!info) throw new Error('bad svg');
        loadImage(info.rasterURL,
          img => setPiece(i, limitPieceSize(img), { svgText: text, viewBox: info.viewBox }));
      })
      .catch(() => loadImage(url, img => setPiece(i, limitPieceSize(img), null)));
  });
}

function preload() {
  Object.entries(CANVAS_MODES).forEach(([name, m]) => { baseImgs[name] = m.img ? loadImage(m.img) : null; });
}

function setup() {
  pixelDensity(displayDensity()); // ★高DPI画面でもマスク/円バッファがジャギらないようにする
  loadParams(); // 保存されたキャンバスモードを、キャンバスを作る前に読み込む
  canvasMode = CANVAS_MODES[PARAMS.canvasMode] ? PARAMS.canvasMode : 'base';
  PARAMS.canvasMode = canvasMode;
  const mode = CANVAS_MODES[canvasMode];
  baseImg = baseImgs[canvasMode];
  const cnv = createCanvas(mode.w, mode.h);
  cnv.parent('sketch-holder');
  setCanvasCss(mode);
  // SVGファイルをキャンバスにドロップして、ピースを並べるパスとして読み込む
  cnv.elt.addEventListener('drop', e => {
    e.preventDefault();
    importPathSvgFile(e.dataTransfer.files[0]);
  });

  // 初期ピースは、起動後にSVGから読み込んで入れる(それまでは空の箱)
  pieces = DEFAULT_PIECES.map(() => null);
  pieceMeta = DEFAULT_PIECES.map(() => null);

  syncParams();
  setupPane();
  setupTilePreview();
  loadDefaultPieces();
  loadDefaultPath();
  buildTintedImages(); // circleColorで着色したtile画像を1回だけ生成する

}

// 画面に収まらない大きさでも全体が見えるよう、表示だけ縮める(座標はp5が補正する)
function setCanvasCss(mode) {
  const holder = document.getElementById('sketch-holder');
  holder.style.setProperty('--canvas-ar', mode.w / mode.h);
  holder.style.setProperty('--canvas-w', mode.w + 'px'); // 原寸より大きくは表示しない
}

// キャンバスモードを切り替える: サイズ・背景画像・バッファを入れ替える
function applyCanvasMode(name) {
  const mode = CANVAS_MODES[name];
  if (!mode) return;
  // スライダーの値はモードごとに別: 前のモードの値を控え、新しいモードの値に入れ替える
  // ★pane.refresh() は、値が変わった項目ごとに 'change' を発火する(Tweakpane 4 の仕様)。
  //   そのたびに saveParams が走ると、新しいモードの値で前のモードの保存を上書きしてしまうので、
  //   切り替え中は保存を止め、終わってから1回だけ保存する
  if (canvasMode) snapshotParams(canvasMode);
  switchingMode = true;
  try {
    applyModeParams(name);
    PARAMS.canvasMode = name;
    canvasMode = name;
    if (pane) pane.refresh();
  } finally {
    switchingMode = false;
  }
  saveParams();
  baseImg = baseImgs[name];
  resizeCanvas(mode.w, mode.h);
  setCanvasCss(mode);
  loadDefaultPath();
}

function draw() {
  syncParams();

  if (USE_TRANSPARENT_BG) {
    clear();
  } else {
    background(bkgdColor);
  }
  if (baseImg && SHOW_IMAGE) image(baseImg, 0, 0, width, height); // 背景画像をキャンバスぴったりに

  beltOffset += BELT_SPEED;

  // 線ごとに「トラック → そのピース」の順で描くので、後から描いた線は、
  // 先に描いた線のピースごと上に重なる
  drawTracksAndPieces();
}

// ------------------------------------------------------------
// ピースのプレビュー(右パネル)
// 箱1つ = ピース1個。画像をドラッグ&ドロップ(またはクリックで選択)すると差し代わる。
// 箱にマウスを乗せると右上に✕が出て、押すと箱ごと削除。下の＋で空の箱を追加する。
// コンベア上には、画像の入っている箱が上から順に交互に流れる。
// ------------------------------------------------------------
function setupTilePreview() {
  // ドロップ先以外に落としたとき、ブラウザが画像を開いてしまうのを防ぐ
  window.addEventListener('dragover', e => e.preventDefault());
  window.addEventListener('drop', e => e.preventDefault());
  renderTilePreview();
  initDrawPad();
}

// ------------------------------------------------------------
// お絵描きキャンバス: 描いた絵を新しいピースとして追加する
// ------------------------------------------------------------
function updatePadAddButton() {
  const btn = document.getElementById('pad-add');
  if (btn) btn.disabled = pieces.length >= MAX_PIECES;
}

function initDrawPad() {
  const cv = document.getElementById('pad-canvas');
  const ctx = cv.getContext('2d');
  const penBtn = document.getElementById('pad-pen');
  const eraserBtn = document.getElementById('pad-eraser');
  const SIZE_MIN = 2, SIZE_MAX = 200;
  let brushSize = 12; // [ ] キーで変える
  const colorInput = document.getElementById('pad-color');
  const fillBtn = document.getElementById('pad-fill');
  let tool = 'pen'; // 'pen' | 'eraser' | 'fill'
  let drawing = false, last = null;

  const setTool = t => {
    tool = t;
    penBtn.classList.toggle('active', t === 'pen');
    eraserBtn.classList.toggle('active', t === 'eraser');
    fillBtn.classList.toggle('active', t === 'fill');
    document.getElementById('pad-cursor').style.visibility = t === 'fill' ? 'hidden' : 'visible';
    cv.style.cursor = t === 'fill' ? 'crosshair' : 'none';
  };
  penBtn.onclick = () => setTool('pen');
  eraserBtn.onclick = () => setTool('eraser');
  fillBtn.onclick = () => setTool('fill');
  document.getElementById('pad-clear').onclick = () => ctx.clearRect(0, 0, cv.width, cv.height);

  const pos = ev => {
    const r = cv.getBoundingClientRect();
    return { x: (ev.clientX - r.left) * cv.width / r.width, y: (ev.clientY - r.top) * cv.height / r.height };
  };
  // アンチエイリアスなしで線を引く: 線分からの距離が半径以内のピクセルだけを、直接塗る
  const stroke = (a, b) => {
    const r = brushSize / 2;
    const hex = colorInput.value;
    const cr = parseInt(hex.slice(1, 3), 16), cg = parseInt(hex.slice(3, 5), 16), cb = parseInt(hex.slice(5, 7), 16);
    const W = cv.width, H = cv.height;
    const x0 = Math.max(0, Math.floor(Math.min(a.x, b.x) - r)), x1 = Math.min(W - 1, Math.ceil(Math.max(a.x, b.x) + r));
    const y0 = Math.max(0, Math.floor(Math.min(a.y, b.y) - r)), y1 = Math.min(H - 1, Math.ceil(Math.max(a.y, b.y) + r));
    if (x1 < x0 || y1 < y0) return;
    const img = ctx.getImageData(x0, y0, x1 - x0 + 1, y1 - y0 + 1);
    const d = img.data, w = img.width;
    const dx = b.x - a.x, dy = b.y - a.y, len2 = dx * dx + dy * dy;
    for (let y = y0; y <= y1; y++) {
      for (let x = x0; x <= x1; x++) {
        const px = x + 0.5, py = y + 0.5;
        let t = len2 === 0 ? 0 : ((px - a.x) * dx + (py - a.y) * dy) / len2;
        t = Math.max(0, Math.min(1, t));
        const ex = px - (a.x + dx * t), ey = py - (a.y + dy * t);
        if (ex * ex + ey * ey > r * r) continue;
        const k = ((y - y0) * w + (x - x0)) * 4;
        if (tool === 'eraser') { d[k] = d[k + 1] = d[k + 2] = d[k + 3] = 0; }
        else { d[k] = cr; d[k + 1] = cg; d[k + 2] = cb; d[k + 3] = 255; }
      }
    }
    ctx.putImageData(img, x0, y0);
  };

  // バケツ: クリックした点と近い色でつながっている範囲を、選んだ色で塗りつぶす
  const bucketFill = (sx, sy) => {
    const W = cv.width, H = cv.height;
    sx = Math.floor(sx); sy = Math.floor(sy);
    if (sx < 0 || sy < 0 || sx >= W || sy >= H) return;
    const img = ctx.getImageData(0, 0, W, H);
    const d = img.data;
    const hex = colorInput.value;
    const fr = parseInt(hex.slice(1, 3), 16), fg = parseInt(hex.slice(3, 5), 16), fb = parseInt(hex.slice(5, 7), 16);
    const i0 = (sy * W + sx) * 4;
    const tr = d[i0], tg = d[i0 + 1], tb = d[i0 + 2], ta = d[i0 + 3];
    if (ta === 255 && tr === fr && tg === fg && tb === fb) return; // すでに同じ色
    const TOL = 0; // アンチエイリアスがないので、完全に同じ色だけを塗る
    const match = i => Math.abs(d[i] - tr) <= TOL && Math.abs(d[i + 1] - tg) <= TOL &&
                       Math.abs(d[i + 2] - tb) <= TOL && Math.abs(d[i + 3] - ta) <= TOL;
    const seen = new Uint8Array(W * H);
    const stack = [sx, sy];
    while (stack.length) {
      const y = stack.pop(), x0 = stack.pop();
      let x = x0;
      while (x >= 0 && !seen[y * W + x] && match((y * W + x) * 4)) x--;
      x++;
      let up = false, down = false;
      while (x < W && !seen[y * W + x] && match((y * W + x) * 4)) {
        const k = y * W + x;
        seen[k] = 1;
        d[k * 4] = fr; d[k * 4 + 1] = fg; d[k * 4 + 2] = fb; d[k * 4 + 3] = 255;
        if (y > 0) {
          const m = !seen[k - W] && match((k - W) * 4);
          if (m && !up) stack.push(x, y - 1);
          up = m;
        }
        if (y < H - 1) {
          const m = !seen[k + W] && match((k + W) * 4);
          if (m && !down) stack.push(x, y + 1);
          down = m;
        }
        x++;
      }
    }
    ctx.putImageData(img, 0, 0);
  };

  cv.addEventListener('pointerdown', ev => {
    if (tool === 'fill') {
      const p = pos(ev);
      bucketFill(p.x, p.y);
      return;
    }
    drawing = true;
    cv.setPointerCapture(ev.pointerId);
    last = pos(ev);
    stroke(last, { x: last.x + 0.01, y: last.y }); // クリックだけでも点が打てる
  });
  cv.addEventListener('pointermove', ev => {
    if (!drawing) return;
    const p = pos(ev);
    stroke(last, p);
    last = p;
  });
  // ブラシの大きさを示す薄いグレーの円(キャンバスの表示倍率に合わせる)
  const ring = document.getElementById('pad-cursor');
  let ringPos = null; // 円を出しているときのマウス位置(キーで大きさを変えたとき再描画する用)
  const moveRing = ev => {
    ringPos = { clientX: ev.clientX, clientY: ev.clientY };
    const r = cv.getBoundingClientRect();
    const d = brushSize * r.width / cv.width;
    ring.style.width = ring.style.height = d + 'px';
    ring.style.left = ev.clientX + 'px';
    ring.style.top = ev.clientY + 'px';
    ring.style.display = 'block';
  };
  cv.addEventListener('pointerenter', moveRing);
  cv.addEventListener('pointermove', moveRing);
  cv.addEventListener('pointerleave', () => { ring.style.display = 'none'; ringPos = null; });

  // [ で小さく、] で大きく(小さいときは細かく、大きいときは粗く刻む)
  window.addEventListener('keydown', ev => {
    if (ev.key !== '[' && ev.key !== ']') return;
    if (ev.metaKey || ev.ctrlKey || ev.altKey) return;
    const cur = brushSize;
    const step = cur < 10 ? 1 : cur < 30 ? 2 : cur < 100 ? 4 : 8;
    brushSize = Math.max(SIZE_MIN, Math.min(SIZE_MAX, cur + (ev.key === ']' ? step : -step)));
    if (ringPos) moveRing(ringPos);
    ev.preventDefault();
  });

  const end = () => { drawing = false; };
  cv.addEventListener('pointerup', end);
  cv.addEventListener('pointercancel', end);

  document.getElementById('pad-add').onclick = () => {
    if (pieces.length >= MAX_PIECES) return;
    // 絵が描かれているかだけ確認する(切り詰めない = キャンバス全体を、描いた位置・大きさのままピースにする)
    const data = ctx.getImageData(0, 0, cv.width, cv.height).data;
    let empty = true;
    for (let k = 3; k < data.length; k += 4) { if (data[k] > 0) { empty = false; break; } }
    if (empty) return; // 何も描かれていない
    const out = cv;

    addPiece();
    const i = pieces.length - 1;
    loadImage(out.toDataURL('image/png'),
      img => setPiece(i, limitPieceSize(img), null),
      () => alert('画像を読み込めませんでした'));
  };
}

// 削除できるのは、箱が2個以上あり、かつ消しても画像入りの箱が1個以上残る場合
function canRemovePiece(i) {
  return pieces.length >= 2 && (!pieces[i] || pieces.some((p, j) => j !== i && p));
}

// piecesの内容から右パネルを作り直す
function renderTilePreview() {
  const holder = document.getElementById('preview-holder');
  holder.innerHTML = '';

  pieces.forEach((img, i) => {
    const slot = document.createElement('div');
    slot.className = 'tile-slot';
    slot.innerHTML =
      `<div class="tile-slot-drop"><div class="tile-slot-thumb"><img alt=""></div>` +
      `<div class="tile-slot-info"><div class="tile-slot-label">Piece${i + 1}</div><div class="tile-slot-hint">DROP IMAGE or CLICK</div></div>` +
      `<button class="tile-slot-close" type="button">✕</button></div>`;
    holder.appendChild(slot);

    const drop = slot.querySelector('.tile-slot-drop');
    if (img) {
      drop.querySelector('img').src = img.canvas.toDataURL();
      drop.classList.add('filled');
    }
    if (canRemovePiece(i)) drop.classList.add('can-remove');

    drop.addEventListener('dragenter', e => { e.preventDefault(); drop.classList.add('over'); });
    drop.addEventListener('dragover',  e => { e.preventDefault(); drop.classList.add('over'); });
    drop.addEventListener('dragleave', () => drop.classList.remove('over'));
    drop.addEventListener('drop', e => {
      e.preventDefault();
      drop.classList.remove('over');
      replaceTile(i, e.dataTransfer.files[0]);
    });
    // ✕ボタン: クリックでファイル選択が開かないようにして、箱ごと削除する
    drop.querySelector('.tile-slot-close').addEventListener('click', e => {
      e.stopPropagation();
      removePiece(i);
    });
    drop.addEventListener('click', () => {
      const input = document.createElement('input');
      input.type = 'file';
      input.accept = 'image/*';
      input.onchange = () => replaceTile(i, input.files[0]);
      input.click();
    });
  });

  updatePadAddButton();

  if (pieces.length < MAX_PIECES) {
    const add = document.createElement('button');
    add.className = 'tile-add';
    add.type = 'button';
    add.textContent = '＋';
    add.addEventListener('click', addPiece);
    holder.appendChild(add);
  }
}

// 空の箱を1つ追加する(画像を入れるまでは描画に使われない)
function addPiece() {
  if (pieces.length >= MAX_PIECES) return;
  pieces.push(null);
  pieceMeta.push(null);
  renderTilePreview();
  const holder = document.getElementById('preview-holder');
  holder.scrollTop = holder.scrollHeight;
}

// 箱を1つ削除する
function removePiece(i) {
  if (!canRemovePiece(i)) return;
  pieces.splice(i, 1);
  pieceMeta.splice(i, 1);
  buildTintedImages();
  renderTilePreview();
}

// 長辺が MAX_PIECE_PX を超えるときだけ、縦横比を保ったまま縮小する。
// 毎フレーム大きな画像を縮小して描くと重いので、読み込み時に1回だけ小さくしておく。
function limitPieceSize(img) {
  const longSide = Math.max(img.width, img.height);
  if (longSide > MAX_PIECE_PX) {
    const k = MAX_PIECE_PX / longSide;
    img.resize(Math.max(1, Math.round(img.width * k)), Math.max(1, Math.round(img.height * k)));
  }
  return img;
}

function replaceTile(i, file) {
  if (!file) return;
  const isSvg = file.type === 'image/svg+xml' || /\.svg$/i.test(file.name);
  if (!isSvg && !file.type.startsWith('image/')) return;

  if (isSvg) {
    // SVGは、画面用にラスター化した画像と、書き出し用の元のSVGの両方を持つ
    file.text().then(text => {
      const info = prepareSvgPiece(text);
      if (!info) { alert('SVGを読み込めませんでした'); return; }
      loadImage(info.rasterURL,
        img => setPiece(i, limitPieceSize(img), { svgText: text, viewBox: info.viewBox }),
        () => alert('SVGを読み込めませんでした'));
    });
    return;
  }

  const reader = new FileReader();
  reader.onload = () => {
    loadImage(reader.result,
      img => setPiece(i, limitPieceSize(img), null),
      () => alert('画像を読み込めませんでした'));
  };
  reader.readAsDataURL(file);
}

function setPiece(i, img, meta) {
  if (i >= pieces.length) return; // 読み込み中に箱が削除された場合
  pieces[i] = img;
  pieceMeta[i] = meta;
  buildTintedImages();
  renderTilePreview();
}

// SVGのviewBox(なければwidth/height)を {minX, minY, w, h} で返す
function readSvgViewBox(root) {
  const nums = (root.getAttribute('viewBox') || '').trim().split(/[\s,]+/).map(parseFloat);
  if (nums.length === 4 && nums.every(Number.isFinite) && nums[2] > 0 && nums[3] > 0) {
    return { minX: nums[0], minY: nums[1], w: nums[2], h: nums[3] };
  }
  const w = parseFloat(root.getAttribute('width'))  || 100;
  const h = parseFloat(root.getAttribute('height')) || 100;
  return { minX: 0, minY: 0, w, h };
}

// SVGの文字列を解析し、画面用にラスター化するためのデータURLと viewBox を返す(失敗したら null)。
// ブラウザ標準の画像読み込みに任せるので、transform・グラデーション・class なども正しく描かれる。
function prepareSvgPiece(text) {
  const doc = new DOMParser().parseFromString(text, 'image/svg+xml');
  const root = doc.documentElement;
  if (!root || root.nodeName.toLowerCase() !== 'svg' || doc.querySelector('parsererror')) return null;

  const viewBox = readSvgViewBox(root);
  const k = MAX_PIECE_PX / Math.max(viewBox.w, viewBox.h); // 長辺が MAX_PIECE_PX になる倍率

  const clone = root.cloneNode(true);
  clone.setAttribute('viewBox', `${viewBox.minX} ${viewBox.minY} ${viewBox.w} ${viewBox.h}`);
  clone.setAttribute('width',  Math.max(1, Math.round(viewBox.w * k)));
  clone.setAttribute('height', Math.max(1, Math.round(viewBox.h * k)));

  const rasterURL = 'data:image/svg+xml;charset=utf-8,' +
    encodeURIComponent(new XMLSerializer().serializeToString(clone));
  return { rasterURL, viewBox };
}

// 今のモードの初期パス(SVG)を読み込む。前のモードの初期パスは取り除く(自分で描いた線・読み込んだ線は残す)
function loadDefaultPath() {
  const name = canvasMode;
  const url = CANVAS_MODES[name].path;
  tracks = tracks.filter(t => !t.isDefault);
  if (!url) return; // blankモードは初期パスなし
  fetch(url)
    .then(r => { if (!r.ok) throw new Error(r.status); return r.text(); })
    .then(text => {
      if (name !== canvasMode) return; // 読み込み中にモードが切り替わった
      tracks = tracks.filter(t => !t.isDefault);
      importPathSvg(text, true);
    })
    .catch(e => console.warn('初期パスを読み込めませんでした: ' + url, e));
}

// ------------------------------------------------------------
// SVGのパスを読み込み、ピースを並べるトラックとして追加する
// SVGはキャンバスに収まる大きさに拡大縮小して、中央に置く
// ------------------------------------------------------------
function importPathSvgFile(file) {
  if (!file) return;
  if (!(file.type === 'image/svg+xml' || /\.svg$/i.test(file.name))) return;
  file.text().then(importPathSvg);
}

function importPathSvg(text, isDefault) {
  const data = parseSvgTracks(text, CURVE_STEPS);
  if (!data || data.subpaths.length === 0) {
    alert('SVGからパス(線)を読み込めませんでした');
    return;
  }

  const vb = data.viewBox;
  const sf = Math.min(width / vb.w, height / vb.h);
  const offsetX = (width  - vb.w * sf) * 0.5;
  const offsetY = (height - vb.h * sf) * 0.5;
  const group = ++trackGroupId;
  let added = 0;

  data.subpaths.forEach(raw => {
    // 同じ位置が続く点は取り除き、ほぼ直線上の余分な点は間引く
    let pts = raw.map(p => ({ x: offsetX + (p.x - vb.minX) * sf, y: offsetY + (p.y - vb.minY) * sf }))
                 .filter((p, i, arr) => i === 0 || Math.hypot(p.x - arr[i - 1].x, p.y - arr[i - 1].y) > 0.01);
    pts = simplifyPolyline(pts, 0.1);
    if (pts.length < 2) return;

    const segLens = [];
    let total = 0;
    for (let i = 0; i < pts.length - 1; i++) {
      const d = Math.hypot(pts[i + 1].x - pts[i].x, pts[i + 1].y - pts[i].y);
      segLens.push(d);
      total += d;
    }
    if (total === 0) return;

    // 始点と終点が一致するパス(Zで閉じたもの)は「閉じた線」にする
    // (pts は最後に始点と同じ点を持ったまま。形を作るときだけ最後の点を除く)
    const closed = pts.length >= 4 && Math.hypot(pts[0].x - pts[pts.length - 1].x, pts[0].y - pts[pts.length - 1].y) < 0.5;
    if (closed) pts[pts.length - 1] = { x: pts[0].x, y: pts[0].y };

    const track = { pts, segLens, total, group, fillable: true, closed, isDefault: !!isDefault }; // fillable: 閉じて塗りつぶせる(SVGから読み込んだ線)
    tracks.push(track);
    added++;
  });

  if (added === 0) alert('SVGからパス(線)を読み込めませんでした');
}

// ------------------------------------------------------------
// マウス操作
// ------------------------------------------------------------
function mousePressed(e) {
  if (e && e.target && e.target.tagName !== 'CANVAS') return;
  if (mouseX < 0 || mouseX > width || mouseY < 0 || mouseY > height) return;

  activeTrack = {
    pts: [{ x: mouseX, y: mouseY }],
    segLens: [],
    total: 0
  };
  tracks.push(activeTrack);
}

function mouseDragged() {
  if (!activeTrack) return;

  const last = activeTrack.pts[activeTrack.pts.length - 1];
  const d = dist(last.x, last.y, mouseX, mouseY);
  if (d < MIN_POINT_DIST) return;

  activeTrack.pts.push({ x: mouseX, y: mouseY });
  activeTrack.segLens.push(d);
  activeTrack.total += d;
}

function mouseReleased() {
  if (!activeTrack) return;

  const track = activeTrack;
  activeTrack = null;

  if (track.pts.length < 2) {
    // クリックだけで線にならなかった場合は捨てる
    tracks.splice(tracks.indexOf(track), 1);
  }
}

// 直前の線を1本戻す。SVGパスをまとめて読み込んだ線は、同じ読み込み分を一度に戻す
function undoTrack() {
  if (activeTrack || tracks.length === 0) return;
  const last = tracks.pop();
  if (!last.group) return;
  while (tracks.length && tracks[tracks.length - 1].group === last.group) {
    tracks.pop();
  }
}

function keyPressed() {
  if (key === 'z') undoTrack();
  if (key === 'c' && !activeTrack) removeAllTracks();
}

// ------------------------------------------------------------
// tile/tile2画像に circleColor でtint(色付け)した版を1回だけ作っておく。
// multiplyでtile画像の色に色を掛け合わせ、destination-inで元の透明度(形)を
// 復元することで、p5のtint()と同様に元の柄・濃淡を保ったまま色だけ変える。
// ------------------------------------------------------------
function buildTintedImages() {
  // 空の箱(null)は null のまま。描画側では画像の入っている箱だけを使う
  tintedPieces = pieces.map(img => img ? tintImage(img, circleColor) : null);
}

function tintImage(img, hexColor) {
  const g = createGraphics(img.width, img.height);
  const raw = img.canvas || img.elt;

  g.image(img, 0, 0, img.width, img.height);

  const ctx = g.drawingContext;
  ctx.globalCompositeOperation = "multiply";
  ctx.fillStyle = hexColor;
  ctx.fillRect(0, 0, img.width, img.height);

  ctx.globalCompositeOperation = "destination-in";
  ctx.drawImage(raw, 0, 0, img.width, img.height);

  ctx.globalCompositeOperation = "source-over";
  const canvas = g.canvas || g.elt;
  g.remove(); // 着色用の一時バッファを解放する(canvas自体は、そのまま描画に使える)
  return canvas;
}

function removeAllTracks() {
  tracks = [];
}

// ------------------------------------------------------------
// 元の頂点だけを直線でつないでトラックを描く(見た目用)
// ------------------------------------------------------------
// 読み込んだSVGの線は、始点と終点をつないで閉じ、中を一色で塗りつぶす(トラックの下に敷く)
function drawPathFill(track) {
  if (track.pts.length < 3) return;
  noStroke();
  fill(FILL_COLOR);
  beginShape();
  track.pts.forEach(p => vertex(p.x, p.y));
  endShape(CLOSE);
}

function drawTrack(track) {
  if (track.pts.length < 2) return;

  noFill();
  stroke(trackColor);
  strokeWeight(TRACK_WIDTH);
  strokeJoin(MITER);
  strokeCap(PROJECT);

  // 閉じた線は、重複している最後の点を除いて CLOSE でつなぐ。開いた線はつながない
  const vs = track.closed ? track.pts.slice(0, -1) : track.pts;
  beginShape();
  vs.forEach(p => vertex(p.x, p.y));
  endShape(track.closed ? CLOSE : undefined);
}

// (idx, i) の組から常に同じ値を返す疑似乱数(0〜1)。
// フレームごとに再計算しても同じタイルは同じ値になる = ジッターがチラつかない
function hashRandom(idx, i) {
  const x = Math.sin(idx * 12.9898 + i * 78.233) * 43758.5453;
  return x - Math.floor(x);
}

function pointAtDistance(track, distance) {
  const { pts, segLens, total } = track;
  if (pts.length < 2 || total === 0) return null;
  if (distance < 0 || distance > total) return null;

  let remaining = distance;
  for (let i = 0; i < segLens.length; i++) {
    if (remaining <= segLens[i]) {
      const t = segLens[i] === 0 ? 0 : remaining / segLens[i];
      const a = pts[i];
      const b = pts[i + 1];
      return {
        x: lerp(a.x, b.x, t),
        y: lerp(a.y, b.y, t),
        angle: Math.atan2(b.y - a.y, b.x - a.x) // ★進行方向の角度を追加
      };
    }
    remaining -= segLens[i];
  }
  const last = pts[pts.length - 1];
  const prev = pts[pts.length - 2];
  return {
    x: last.x, y: last.y,
    angle: Math.atan2(last.y - prev.y, last.x - prev.x)
  };
}

function pointAtExtendedDistance(track, distance) {
  const { pts, total } = track;
  if (pts.length < 2) return { x: pts[0].x, y: pts[0].y, angle: 0 };

  if (distance < 0) {
    const a = pts[0], b = pts[1];
    const dx = a.x - b.x, dy = a.y - b.y;
    const len = Math.hypot(dx, dy) || 1;
    const t = -distance;
    return {
      x: a.x + (dx / len) * t,
      y: a.y + (dy / len) * t,
      angle: Math.atan2(b.y - a.y, b.x - a.x) // 始点側の向き
    };
  }
  if (distance > total) {
    const a = pts[pts.length - 2], b = pts[pts.length - 1];
    const dx = b.x - a.x, dy = b.y - a.y;
    const len = Math.hypot(dx, dy) || 1;
    const t = distance - total;
    return {
      x: b.x + (dx / len) * t,
      y: b.y + (dy / len) * t,
      angle: Math.atan2(dy, dx) // 終点側の向き
    };
  }
  return pointAtDistance(track, distance);
}

// ------------------------------------------------------------
// 線ごとに「塗りつぶし → トラックの縁取り → ピース」の順に、キャンバスへ直接描く。
// ピースは線の形でクリップしない(track w は、縁取りの太さだけに効く)。
// ------------------------------------------------------------
function drawTracksAndPieces() {
  const items = getPieceItems();

  const drawH = CIRCLE_DIAMETER;
  const halfH = drawH / 2;

  // ★p5のpush/translate/rotate/image(状態管理のオーバーヘッドが大きい)を使わず、
  //   生のCanvas2D APIを直接呼ぶことで、画像枚数が多いときの描画コストを下げる
  // ★元画像ではなく、circleColorでtint済みの版(tintedPieces)を描画に使う
  const ctx = drawingContext;

  tracks.forEach((track, idx) => {
    if (FILL_PATH && track.fillable) drawPathFill(track);
    if (SHOW_TRACK) drawTrack(track);
    if (items.length === 0) return;
    if (track.total === 0) return;

    forEachPlacement(track, idx, items.length, (n, x, y, angle) => {
      const item = items[n];
      ctx.save();
      ctx.translate(x, y);
      ctx.rotate(angle); // ★進行方向に向けて回転
      ctx.drawImage(item.raw, -item.w / 2, -halfH, item.w, drawH);
      ctx.restore();
    });
  });
}

// ★画像の縦横比を保ったまま、高さをCIRCLE_DIAMETERに合わせた描画幅つきで、
//   画像の入っている箱だけを返す。画面描画とSVG書き出しで共有する。
//   k = piecesの添字、raw = 着色済みCanvas、w = 描画幅(px)
function getPieceItems() {
  const drawH = CIRCLE_DIAMETER;
  const items = [];
  pieces.forEach((img, k) => {
    if (img && tintedPieces[k]) items.push({ k, raw: tintedPieces[k], w: drawH * (img.width / img.height) });
  });
  return items;
}

// ピースの出現割合。Piece番号(0始まり)ごとの重みで、書いていない箱は 1。
// 今は全部均等(重み 1)。例: [1, 2] なら Piece2 が2倍の割合で出る。
const PIECE_WEIGHTS = [];

// 使うピース(getPieceItems の添字)を、重みの回数だけ並べた巡回パターンを返す。
// 交互モードはこの並びを順に使い、ランダムモードはこの並びから選ぶ。
// 例: 重みが 2,2,1,1 → [0,1,2,3,0,1]。同じピースが固まらないよう、1周ずつ回して並べる
function piecePattern() {
  const items = getPieceItems();
  const w = items.map(it => PIECE_WEIGHTS[it.k] ?? 1);
  const pattern = [];
  const rounds = Math.max(1, ...w);
  for (let r = 0; r < rounds; r++) {
    w.forEach((wi, i) => { if (wi > r) pattern.push(i); });
  }
  return pattern.length ? pattern : [0];
}

// ランダム順で、同じピースが続く数の上限
const MAX_RUN = 3; // 4個以上は並ばない

// 線ごとの乱数列のキャッシュ。slotId>=0 は fwd[slotId]、slotId<0 は bwd[-1-slotId] に入れる。
// 前の MAX_RUN 個が同じピースなら、同じものを選ばない、という規則を保ったまま
// 固定の乱数で順に決める(同じ slotId は、いつでも同じピースになる)。
const randomSeqCache = new Map();

function randomPick(trackIdx, slotId, pattern) {
  const key = `${trackIdx}|${RANDOM_SEED}|${pattern.join(',')}`;
  let c = randomSeqCache.get(key);
  if (!c) {
    if (randomSeqCache.size > 64) randomSeqCache.clear(); // シードを動かしたときの古い列を捨てる
    c = { fwd: [], bwd: [] };
    randomSeqCache.set(key, c);
  }
  const L = pattern.length;
  const distinct = new Set(pattern).size;
  const base = trackIdx + 2000 + RANDOM_SEED * 101;

  // slotId の位置に置くピースを、neighbors(MAX_RUN 個の隣)を避けて決める
  const choose = (sid, neighbors) => {
    const blocked = neighbors.length === MAX_RUN && neighbors.every(v => v === neighbors[0]) && distinct > 1
      ? neighbors[0] : -1;
    for (let salt = 0; salt < 8; salt++) {
      const v = pattern[Math.min(L - 1, Math.floor(hashRandom(base + salt * 1009, sid) * L))];
      if (v !== blocked) return v;
    }
    return pattern.find(v => v !== blocked); // 乱数で避けきれなかったときの保険
  };
  const at = sid => (sid >= 0 ? c.fwd[sid] : c.bwd[-1 - sid]);

  // 前方(slotId>=0)は、直前の MAX_RUN 個を見ながら順に伸ばす
  const growFwd = upto => {
    while (c.fwd.length <= upto) {
      const k = c.fwd.length;
      c.fwd.push(choose(k, c.fwd.slice(Math.max(0, k - MAX_RUN), k)));
    }
  };
  growFwd(Math.max(slotId, MAX_RUN));

  if (slotId >= 0) return c.fwd[slotId];

  // 後方(slotId<0)は、0 に近い側の MAX_RUN 個を見ながら外側へ伸ばす
  while (c.bwd.length < -slotId) {
    const sid = -1 - c.bwd.length;
    const neighbors = [];
    for (let d = 1; d <= MAX_RUN; d++) neighbors.push(at(sid + d));
    c.bwd.push(choose(sid, neighbors));
  }
  return c.bwd[-1 - slotId];
}

// ------------------------------------------------------------
// トラック上の現在のピース配置を1つずつ fn(n, x, y, angle) に渡す。
// n = 何番目のピースを使うか(0〜nItems-1)、x/y = ジッター込みの中心座標、angle = 進行方向。
// 画面描画とSVG書き出しで同じ配置を使うための共通処理。
// ------------------------------------------------------------
function forEachPlacement(track, idx, nItems, fn) {
  const pattern = piecePattern();
  const spacing = CIRCLE_DIAMETER + CIRCLE_GAP;
  const total = track.total;
  if (total === 0) return;

  const closed = !!track.closed;
  const extra = closed ? 0 : CIRCLE_DIAMETER; // 端の外側にもはみ出させる余白(閉じた線ははみ出さず、周回する)
  const phase = ((beltOffset % spacing) + spacing) % spacing;

  const iMin = Math.floor((-extra - phase) / spacing);
  const iMax = Math.ceil((total + extra - phase) / spacing);

  // ★何周目か(beltOffsetがspacingを何回超えたか)。jitterのスロット番号から
  //   巡回による付け替わりを打ち消すために使う
  const beltStep = Math.floor(beltOffset / spacing);

  for (let i = iMin; i <= iMax; i++) {
    const distAlong = i * spacing + phase;
    if (distAlong < -extra || distAlong > total + extra) continue;
    if (closed && distAlong >= total) continue; // 閉じた線は、終点=始点なので重ねて置かない

    const p = pointAtExtendedDistance(track, distAlong);
    if (!p) continue;

    // ★進行方向(p.angle)に垂直な法線方向へ、タイルごとに固定のランダム量だけずらす
    // iはベルト一周ごとに巡回して使い回される「スロット番号」でしかないため、
    // そのまま使うと同じ場所を流れるタイルのジッターが周期的に変わってしまう。
    // beltStep(周回数)を差し引いた slotId は巡回に依存しない固定の識別子になる。
    const slotId = i - beltStep;
    const normalAngle = p.angle + Math.PI / 2;

    // ★低周波のnoise()で「揺れる区間/揺れない区間」のゾーンを作り、
    //   区間の中だけhashRandomの揺れを効かせることでコントラストを出す。
    //   境界だけZONE_SOFTNESSの幅でなめらかにして、タイル単位でのパチパチした
    //   切り替わりを防ぐ(ZONE_SOFTNESSを0に近づけるほどくっきりしたON/OFFになる)。
    const zoneNoise = noise(idx * 500 + slotId * ZONE_SCALE);
    const gate = constrain((zoneNoise - (ZONE_THRESHOLD - ZONE_SOFTNESS)) / (2 * ZONE_SOFTNESS), 0, 1);

    const jitter = gate * (hashRandom(idx, slotId) * 2 - 1) * JITTER_AMOUNT;
    const jx = p.x + Math.cos(normalAngle) * jitter;
    const jy = p.y + Math.sin(normalAngle) * jitter;

    // ★通常はスロット番号でピースを巡回して交互に使う(2個なら従来のtile/tile2の交互配置)
    // ★向きは、進行方向から PIECE_ROTATION 度だけ一律に回す
    const rot = PIECE_ROTATION * Math.PI / 180;
    // ★ランダム順モードでは、スロット番号から固定の乱数で使うピースを選ぶ
    //   (slotIdは巡回に依存しないので、流れても同じ場所のピースは変わらない)
    //   どちらのモードも、出現割合の重み(PIECE_WEIGHTS)を反映した並びから選ぶ
    const L = pattern.length;
    //   ランダム順は、同じピースが MAX_RUN 個より多く続かないように選ぶ(randomPick)
    const n = RANDOM_ORDER
      ? randomPick(idx, slotId, pattern)
      : pattern[((slotId % L) + L) % L];
    fn(n, jx, jy, p.angle + rot);
  }
}

// ============================================================
// SVG書き出し(Illustrator等で編集する用)
//
// 現在のフレームの配置をそのまま書き出す。構成:
//   <defs>  ピースの定義(SVGピースは元のSVGの中身、PNGなどは<image>で埋め込み)
//   背景・トラックの縁取り
//   線ごとに <g> でまとめ、中に <use> でピースを配置(線の形でのクリップはしない)
// ============================================================
function exportSVG() {
  const items = getPieceItems();
  const liveTracks = tracks.filter(t => t !== activeTrack && t.pts.length >= 2 && t.total > 0);
  // 白い縁取りとクリップは、同じ間引き後の点を使う(別々だと縁がずれて見える)
  // 閉じた線は、重複している最後の点を除いた頂点列にする(形は closed 指定で閉じる)
  const simplified = new Map(liveTracks.map(t => {
    const sp = simplifyPolyline(t.pts, SIMPLIFY_EPSILON);
    return [t, t.closed ? sp.slice(0, -1) : sp];
  }));
  if (items.length === 0 || liveTracks.length === 0) {
    alert('書き出す線がありません');
    return;
  }

  const f = v => Math.round(v * 100) / 100;
  const drawH = CIRCLE_DIAMETER;
  const tint = color(circleColor).levels;
  const needsTint = tint[0] < 255 || tint[1] < 255 || tint[2] < 255;
  const serializer = new XMLSerializer();

  // --- ピースの定義 ---
  let defs = '';
  items.forEach(it => {
    const id = 'piece-' + it.k;
    const meta = pieceMeta[it.k];
    if (meta) {
      const root = new DOMParser().parseFromString(meta.svgText, 'image/svg+xml').documentElement;
      if (needsTint) tintSvgColors(root, tint);
      const inner = Array.from(root.childNodes).map(n => serializer.serializeToString(n)).join('');
      const vb = meta.viewBox;
      const s = drawH / vb.h;
      // ピースの中心を原点に置き、高さがdrawHになるよう拡大縮小する
      defs += `<g id="${id}" transform="scale(${s}) translate(${f(-(vb.minX + vb.w / 2))} ${f(-(vb.minY + vb.h / 2))})">${inner}</g>\n`;
    } else {
      // ラスター画像は、着色済みの画像をそのまま埋め込む(ベクターにはならない)
      defs += `<image id="${id}" x="${f(-it.w / 2)}" y="${f(-drawH / 2)}" width="${f(it.w)}" height="${f(drawH)}" ` +
              `xlink:href="${it.raw.toDataURL('image/png')}"/>\n`;
    }
  });

  // --- 線ごとのクリップとピース配置 ---
  // 線ごとに「トラックの縁取り → そのピース」の順に積む(後の線が上に重なる。画面と同じ)
  let body = '';
  if (!USE_TRANSPARENT_BG) {
    body += `<rect id="background" width="${width}" height="${height}" fill="${bkgdColor}"/>\n`;
  }
  if (baseImg && SHOW_IMAGE) {
    body += `<image id="base" x="0" y="0" width="${width}" height="${height}" preserveAspectRatio="none" ` +
            `xlink:href="${baseImg.canvas.toDataURL('image/jpeg', 0.92)}"/>\n`;
  }
  liveTracks.forEach((track, n) => {
    if (FILL_PATH && track.fillable && track.pts.length >= 3) {
      const fpts = simplified.get(track).map(p => `${f(p.x)},${f(p.y)}`).join(' ');
      body += `<polygon id="fill-${n}" points="${fpts}" fill="${FILL_COLOR}"/>\n`; // polygonは自動で始点と終点がつながる
    }
    if (SHOW_TRACK) {
      const pts = simplified.get(track).map(p => `${f(p.x)},${f(p.y)}`).join(' ');
      // 閉じた線は polygon(継ぎ目もマイター結合)、開いた線は polyline(端は四角く延長)
      body += `<${track.closed ? 'polygon' : 'polyline'} id="track-${n}" points="${pts}" fill="none" stroke="${trackColor}" stroke-width="${TRACK_WIDTH}" ` +
              `stroke-linejoin="miter" stroke-miterlimit="10" stroke-linecap="square"/>\n`;
    }
    const idx = tracks.indexOf(track);
    let uses = '';
    forEachPlacement(track, idx, items.length, (n, x, y, angle) => {
      uses += `<use xlink:href="#piece-${items[n].k}" transform="translate(${f(x)} ${f(y)}) rotate(${f(angle * 180 / Math.PI)})"/>\n`;
    });
    if (!uses) return;
    body += `<g id="line-${idx}">\n${uses}</g>\n`;
  });

  const svg =
    `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" ` +
    `width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">\n` +
    `<defs>\n${defs}</defs>\n${body}</svg>\n`;

  const blob = new Blob([svg], { type: 'image/svg+xml' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'play-with-maker.svg';
  a.click();
  URL.revokeObjectURL(url);
}

// Ramer–Douglas–Peucker法で、折れ線を許容誤差 epsilon(px)以内で単純化する。
// ドラッグで4pxごとに打った点を間引いて、線の形のクリップを軽くする。
function simplifyPolyline(pts, epsilon) {
  if (pts.length < 3) return pts;
  const keep = new Uint8Array(pts.length);
  keep[0] = keep[pts.length - 1] = 1;
  const stack = [[0, pts.length - 1]];
  while (stack.length) {
    const [lo, hi] = stack.pop();
    const a = pts[lo], b = pts[hi];
    const dx = b.x - a.x, dy = b.y - a.y;
    const len = Math.hypot(dx, dy);
    let maxD = 0, maxI = -1;
    for (let i = lo + 1; i < hi; i++) {
      const d = len === 0
        ? Math.hypot(pts[i].x - a.x, pts[i].y - a.y)
        : Math.abs(dy * (pts[i].x - a.x) - dx * (pts[i].y - a.y)) / len;
      if (d > maxD) { maxD = d; maxI = i; }
    }
    if (maxD > epsilon) {
      keep[maxI] = 1;
      stack.push([lo, maxI], [maxI, hi]);
    }
  }
  return pts.filter((_, i) => keep[i]);
}

// SVGの色指定(fill / stroke / stop-color / flood-color と style 属性内)に、
// 画面と同じ乗算の着色を掛けて書き換える。none / url(...) / currentColor はそのまま。
function tintSvgColors(root, tint) {
  const COLOR_PROPS = ['fill', 'stroke', 'stop-color', 'flood-color'];
  [root, ...root.querySelectorAll('*')].forEach(el => {
    COLOR_PROPS.forEach(prop => {
      const v = el.getAttribute(prop);
      if (v) el.setAttribute(prop, multiplyColor(v, tint));
    });
    const style = el.getAttribute('style');
    if (style) {
      el.setAttribute('style', style.split(';').map(decl => {
        const idx = decl.indexOf(':');
        if (idx === -1) return decl;
        const prop = decl.slice(0, idx).trim();
        return COLOR_PROPS.includes(prop) ? `${prop}:${multiplyColor(decl.slice(idx + 1).trim(), tint)}` : decl;
      }).join(';'));
    }
  });
}

function multiplyColor(value, tint) {
  const v = value.trim();
  if (/^(none|transparent|currentcolor|inherit)$/i.test(v) || v.startsWith('url(')) return value;
  try {
    const c = color(v).levels;
    const hex = n => Math.round(n).toString(16).padStart(2, '0');
    return '#' + hex(c[0] * tint[0] / 255) + hex(c[1] * tint[1] / 255) + hex(c[2] * tint[2] / 255);
  } catch (e) {
    return value;
  }
}
