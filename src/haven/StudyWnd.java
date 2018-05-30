package haven;

import haven.resutil.Curiosity;

public class StudyWnd extends GameUI.Hidewnd {
	InventoryProxy study;
	StudyInfo info;

	StudyWnd() {
		super(Coord.z, "Study");
	}

	public void setStudy(Inventory inventory) {
		if (study != null) {
			study.reqdestroy();
			info.reqdestroy();
		}
		final InventoryProxy invp = new InventoryProxy(inventory);
		study = add(invp, 0, 20);
		info = add(new StudyInfo(new Coord(study.sz.x, 50), inventory), 0, study.c.y + study.sz.y + 5);
		pack();
	}

	private static class StudyInfo extends Widget {
		public Widget study;
		public int texp, tw, tenc;
		private final Text.UText<?> texpt = new Text.UText<Integer>(Text.std) {
			public Integer value() {
				return (texp);
			}

			public String text(Integer v) {
				return (Utils.thformat(v));
			}
		};
		private final Text.UText<?> twt = new Text.UText<String>(Text.std) {
			public String value() {
				return (tw + "/" + ui.sess.glob.cattr.get("int").comp);
			}
		};
		private final Text.UText<?> tenct = new Text.UText<Integer>(Text.std) {
			public Integer value() {
				return (tenc);
			}

			public String text(Integer v) {
				return (Integer.toString(tenc));
			}
		};

		private StudyInfo(Coord sz, Widget study) {
			super(sz);
			this.study = study;
			add(new Label("Attention:"), 2, 2);
			add(new Label("Experience cost:"), 2, 18);
			add(new Label("Learning points:"), 2, 34);
		}

		private void upd() {
			int texp = 0, tw = 0, tenc = 0;
			for (GItem item : study.children(GItem.class)) {
				try {
					Curiosity ci = ItemInfo.find(Curiosity.class, item.info());
					if (ci != null) {
						texp += ci.exp;
						tw += ci.mw;
						tenc += ci.enc;
					}
				} catch (Loading ignored) {
				}
			}
			this.texp = texp;
			this.tw = tw;
			this.tenc = tenc;
		}

		public void draw(GOut g) {
			upd();
			super.draw(g);
			g.chcolor(255, 192, 255, 255);
			g.aimage(twt.get().tex(), new Coord(sz.x - 4, 2), 1.0, 0.0);
			g.chcolor(255, 255, 192, 255);
			g.aimage(tenct.get().tex(), new Coord(sz.x - 4, 18), 1.0, 0.0);
			g.chcolor(192, 192, 255, 255);
			g.aimage(texpt.get().tex(), new Coord(sz.x - 4, 34), 1.0, 0.0);
		}
	}
}