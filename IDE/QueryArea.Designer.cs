namespace Prolog
{
    partial class QueryArea
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        
        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(QueryArea));
            this.checkBox1 = new System.Windows.Forms.CheckBox();
            this.queryPanel = new System.Windows.Forms.Panel();
            this.bgwExecuteQuery = new System.ComponentModel.BackgroundWorker();
            this.debugBtn = new System.Windows.Forms.Button();
            this.btnCancelQuery = new System.Windows.Forms.Button();
            this.btnMore = new System.Windows.Forms.Button();
            this.btnStop = new System.Windows.Forms.Button();
            this.btnXeqQuery = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // checkBox1
            // 
            this.checkBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.checkBox1.AutoSize = true;
            this.checkBox1.Location = new System.Drawing.Point(1010, 93);
            this.checkBox1.Name = "checkBox1";
            this.checkBox1.Size = new System.Drawing.Size(103, 17);
            this.checkBox1.TabIndex = 37;
            this.checkBox1.Text = "Callstack on Fail";
            this.checkBox1.UseVisualStyleBackColor = true;
            // 
            // queryPanel
            // 
            this.queryPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.queryPanel.Location = new System.Drawing.Point(5, 4);
            this.queryPanel.Name = "queryPanel";
            this.queryPanel.Size = new System.Drawing.Size(969, 198);
            this.queryPanel.TabIndex = 36;
            // 
            // bgwExecuteQuery
            // 
            this.bgwExecuteQuery.WorkerReportsProgress = true;
            this.bgwExecuteQuery.WorkerSupportsCancellation = true;
            this.bgwExecuteQuery.DoWork += new System.ComponentModel.DoWorkEventHandler(this.bgwExecuteQuery_DoWork);
            this.bgwExecuteQuery.ProgressChanged += new System.ComponentModel.ProgressChangedEventHandler(this.bgwExecuteQuery_ProgressChanged);
            this.bgwExecuteQuery.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(this.bgwExecuteQuery_RunWorkerCompleted);
            // 
            // debugBtn
            // 
            this.debugBtn.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.debugBtn.BackColor = System.Drawing.SystemColors.Control;
            this.debugBtn.Image = ((System.Drawing.Image)(resources.GetObject("debugBtn.Image")));
            this.debugBtn.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.debugBtn.Location = new System.Drawing.Point(981, 46);
            this.debugBtn.Margin = new System.Windows.Forms.Padding(4);
            this.debugBtn.Name = "debugBtn";
            this.debugBtn.Size = new System.Drawing.Size(106, 40);
            this.debugBtn.TabIndex = 39;
            this.debugBtn.TabStop = false;
            this.debugBtn.Text = "       Debug";
            this.debugBtn.UseVisualStyleBackColor = true;
            this.debugBtn.Click += new System.EventHandler(this.debugBtn_Click);
            // 
            // btnCancelQuery
            // 
            this.btnCancelQuery.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCancelQuery.BackColor = System.Drawing.SystemColors.Control;
            this.btnCancelQuery.Enabled = false;
            this.btnCancelQuery.Image = ((System.Drawing.Image)(resources.GetObject("btnCancelQuery.Image")));
            this.btnCancelQuery.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancelQuery.Location = new System.Drawing.Point(1093, 88);
            this.btnCancelQuery.Margin = new System.Windows.Forms.Padding(4);
            this.btnCancelQuery.Name = "btnCancelQuery";
            this.btnCancelQuery.Size = new System.Drawing.Size(76, 40);
            this.btnCancelQuery.TabIndex = 38;
            this.btnCancelQuery.Text = "Halt";
            this.btnCancelQuery.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancelQuery.UseVisualStyleBackColor = true;
            this.btnCancelQuery.Visible = false;
            this.btnCancelQuery.Click += new System.EventHandler(this.btnCancelQuery_Click);
            // 
            // btnMore
            // 
            this.btnMore.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnMore.BackColor = System.Drawing.SystemColors.Control;
            this.btnMore.Image = ((System.Drawing.Image)(resources.GetObject("btnMore.Image")));
            this.btnMore.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnMore.Location = new System.Drawing.Point(1093, 4);
            this.btnMore.Margin = new System.Windows.Forms.Padding(4);
            this.btnMore.Name = "btnMore";
            this.btnMore.Size = new System.Drawing.Size(76, 40);
            this.btnMore.TabIndex = 33;
            this.btnMore.Text = "More";
            this.btnMore.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnMore.UseVisualStyleBackColor = true;
            this.btnMore.Click += new System.EventHandler(this.btnMore_Click);
            // 
            // btnStop
            // 
            this.btnStop.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnStop.BackColor = System.Drawing.SystemColors.Control;
            this.btnStop.Image = ((System.Drawing.Image)(resources.GetObject("btnStop.Image")));
            this.btnStop.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnStop.Location = new System.Drawing.Point(1093, 46);
            this.btnStop.Margin = new System.Windows.Forms.Padding(4);
            this.btnStop.Name = "btnStop";
            this.btnStop.Size = new System.Drawing.Size(76, 40);
            this.btnStop.TabIndex = 34;
            this.btnStop.Text = "Stop";
            this.btnStop.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnStop.UseVisualStyleBackColor = true;
            this.btnStop.Click += new System.EventHandler(this.btnStop_Click);
            // 
            // btnXeqQuery
            // 
            this.btnXeqQuery.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnXeqQuery.BackColor = System.Drawing.SystemColors.Control;
            this.btnXeqQuery.Image = ((System.Drawing.Image)(resources.GetObject("btnXeqQuery.Image")));
            this.btnXeqQuery.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnXeqQuery.Location = new System.Drawing.Point(981, 4);
            this.btnXeqQuery.Margin = new System.Windows.Forms.Padding(4);
            this.btnXeqQuery.Name = "btnXeqQuery";
            this.btnXeqQuery.Size = new System.Drawing.Size(106, 40);
            this.btnXeqQuery.TabIndex = 32;
            this.btnXeqQuery.TabStop = false;
            this.btnXeqQuery.Text = "       Query";
            this.btnXeqQuery.UseVisualStyleBackColor = true;
            this.btnXeqQuery.Click += new System.EventHandler(this.btnXeqQuery_Click);
            // 
            // QueryArea
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1173, 205);
            this.Controls.Add(this.debugBtn);
            this.Controls.Add(this.btnCancelQuery);
            this.Controls.Add(this.btnMore);
            this.Controls.Add(this.btnStop);
            this.Controls.Add(this.queryPanel);
            this.Controls.Add(this.btnXeqQuery);
            this.Controls.Add(this.checkBox1);
            this.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.Name = "QueryArea";
            this.Text = "QueryArea";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        

        private System.Windows.Forms.CheckBox checkBox1;
        private System.Windows.Forms.Panel queryPanel;
        private System.Windows.Forms.Button btnStop;
        private System.Windows.Forms.Button btnMore;
        private System.Windows.Forms.Button btnXeqQuery;
        private System.ComponentModel.BackgroundWorker bgwExecuteQuery;
        private System.Windows.Forms.Button btnCancelQuery;
        private System.Windows.Forms.Button debugBtn;
    }
}